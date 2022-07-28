from snakemake.utils import min_version

configfile: "config/default.yaml"
min_version("7.8")


rule all:
    message: "Run entire analysis and compile report."
    input:
        "build/test-report.html"


rule models:
    input:
        data = "data/Survey_data.csv",
        follow_up_data = "data/Survey_data_follow-up.csv",
        tic_matched3 = "data/tic_matched3.xlsx"
    output:
        figshare_data = "build/data_figshare.xlsx"
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
