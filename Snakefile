from snakemake.utils import min_version

configfile: "config/default.yaml"
min_version("7.8")


rule all:
    message: "Run entire analysis and compile report."
    input:
        "build/random-forest.png",
        "build/logistic-regression.png",
        "build/treatment.png",
        "build/test-report.html"


rule preprocess:
    message: "Preprocess raw survey data."
    input:
        data = "data/Survey_data.csv"
    output:
        figshare_data = "build/figshare-data.xlsx",
        preprocessed_data = "build/preprocessed.feather"
    conda: "envs/default.yaml"
    script: "scripts/preprocessing.R"


rule random_forest:
    message: "Train a random forest to data."
    input:
        data = "build/preprocessed.feather"
    output:
        variable_importance = "build/random-forest-variable-importance.feather",
        plot = "build/random-forest-variable-importance.png",
        imputed_data = "build/imputed.feather"
    conda: "envs/default.yaml"
    script: "scripts/random_forest.R"


rule logistic_regression:
    message: "Build logistic regression model."
    input:
        data = "build/preprocessed.feather",
        imputed_data = "build/imputed.feather",
    output:
        plot = "build/logistic-regression.png",
        summary = "build/logistic-regression.feather"
    conda: "envs/default.yaml"
    script: "scripts/logit.R"


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


rule clean: # removes all generated results
    message: "Remove all build results but keep downloaded data."
    run:
         import shutil

         shutil.rmtree("build")
         print("Data downloaded to data/ has not been cleaned.")


rule test:
    conda: "envs/test.yaml"
    output: "build/test-report.html"
    shell:
        "py.test --html={output} --self-contained-html"
