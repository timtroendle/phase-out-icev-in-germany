from snakemake.utils import min_version

configfile: "config/default.yaml"
min_version("7.8")


rule all:
    message: "Run entire analysis and compile report."
    input:
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


rule models:
    input:
        data = "build/preprocessed.feather",
        follow_up_data = "data/Survey_data_follow-up.csv",
        tic_matched3 = "data/tic_matched3.xlsx"
    output: "build/dummy.feather"
    conda: "envs/default.yaml"
    script: "scripts/survey_models.R"


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
