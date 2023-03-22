from snakemake.utils import min_version

configfile: "config/default.yaml"
min_version("7.8")


rule all:
    message: "Run entire analysis and compile report."
    input:
        "build/random-forest-variable-importance.png",
        "build/logistic-regression.png",
        "build/all-logistic-regressions.png",
        "build/nofactor-regression.png",
        "build/treatment.png",
        "build/policy-instrument-p-values-accept.csv",
        "build/policy-instrument-p-values-approve.csv",
        "build/sample-vs-population.csv"


rule preprocess:
    message: "Preprocess raw survey data."
    input:
        data = "data/Survey_data.csv"
    output:
        figshare_data = "build/figshare-data.xlsx",
        preprocessed_data = "build/preprocessed.feather"
    conda: "envs/default.yaml"
    script: "scripts/preprocessing.R"


rule impute:
    message: "Impute missing data."
    input:
        data = "build/preprocessed.feather"
    output:
        imputed_data = "build/imputed.feather"
    conda: "envs/default.yaml"
    script: "scripts/impute.R"


rule factors:
    message: "Perform confirmatory factor analysis and derive factors."
    input:
        imputed_data = "build/imputed.feather"
    output:
        imputed_data_with_factors = "build/imputed-and-factors.feather",
        summary = "build/cfa.txt"
    conda: "envs/lavaan.yaml"
    script: "scripts/cfa.R"


rule random_forest:
    message: "Train a random forest to data."
    input:
        imputed_data = "build/imputed-and-factors.feather"
    params:
        colours = config["colours"]
    output:
        variable_importance = "build/random-forest-variable-importance.feather",
        plot = "build/random-forest-variable-importance.png",
    conda: "envs/default.yaml"
    script: "scripts/random_forest.R"


rule logistic_regression:
    message: "Build logistic regression model."
    input:
        imputed_data = "build/imputed-and-factors.feather",
    params:
        colours = config["colours"]
    output:
        plot = "build/logistic-regression.png",
        summary = "build/logistic-regression.feather",
        coefficients = "build/logistic-regression.txt"
    conda: "envs/default.yaml"
    script: "scripts/logit.R"


rule all_logistic_regressions:
    message: "Build and plot all four logistic regression models."
    input:
        imputed_data = "build/imputed-and-factors.feather",
    params:
        colours = config["colours"]
    output:
        plot = "build/all-logistic-regressions.png",
        data = "build/all-logistic-regressions.feather",
        coefficients1 = "build/all-logistic-regressions1.txt",
        coefficients2 = "build/all-logistic-regressions2.txt",
        coefficients3 = "build/all-logistic-regressions3.txt",
        coefficients4 = "build/all-logistic-regressions4.txt",
    conda: "envs/default.yaml"
    script: "scripts/all_logits.R"


rule no_factor_regrssion:
    message: "Run a regression without factors derived as reference."
    input:
        data = "build/imputed.feather"
    output:
        plot = "build/nofactor-regression.png",
        data = "build/nofactor-regression.feather"
    conda: "envs/default.yaml"
    script: "scripts/nofactor.R"


rule treatments:
    message: "Test the effect of treatments."
    input:
        data = "build/preprocessed.feather",
        follow_up_data = "data/Survey_data_follow-up.csv",
        tic_matched3 = "data/tic_matched3.xlsx"
    output:
        plot = "build/treatment.png"
    conda: "envs/default.yaml"
    script: "scripts/treatments.R"


rule policy_instruments:
    message: "Test difference between policy instrument ratings."
    input:
        data = "build/preprocessed.feather"
    output:
        accept = "build/policy-instrument-p-values-accept.csv",
        approve = "build/policy-instrument-p-values-approve.csv",
    conda:
        "envs/default.yaml"
    script:
        "scripts/policy_instruments.R"


rule census_data:
    message: "Download zensus data to compare our sample against."
    params:
        url = config["data-sources"]["census"]
    output:
        zip = temp("data/automatic/raw-census.zip"),
        csv = protected("data/automatic/Zensus11_Datensatz_Bevoelkerung.csv")
    shadow:
        "minimal"
    shell: """
        curl -sLo {output.zip} '{params.url}'
        unzip -o {output.zip} -d data/automatic
        """


rule sample_vs_population:
    message: "Compare sample to population."
    input:
        sample = "build/preprocessed.feather",
        population = rules.census_data.output.csv
    output:
        "build/sample-vs-population.csv"
    conda:
        "envs/default-py.yaml"
    script:
        "scripts/sample.py"


rule dag:
     message: "Plot dependency graph of the workflow."
     output:
         dot = "build/dag.dot",
         pdf = "build/dag.pdf"
     conda: "envs/dag.yaml"
     shell:
         """
         snakemake --rulegraph > {output.dot}
         dot -Tpdf -o {output.pdf} {output.dot}
         """


rule push:
    message: "Copy entire build to push folder."
    params: push_directory = config["push-directory"]
    shell:
        """
        cp -r build/*.* {params.push_directory}
        """


rule clean: # removes all generated results
    message: "Remove all build results but keep downloaded data."
    run:
         import shutil

         shutil.rmtree("build")
         print("Data downloaded to data/ has not been cleaned.")
