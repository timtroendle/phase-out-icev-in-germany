import re
from io import StringIO

import pandas as pd

POP_ALL = "DEM_1.1"
POP_ALL_MALE = "DEM_1.2"
POP_ALL_FEMALE = "DEM_1.3"
POP_18_24_ALL = "DEM_4.16"
POP_25_29_ALL = "DEM_4.19"
POP_30_39_ALL = "DEM_4.22"
POP_40_49_ALL = "DEM_4.25"
POP_50_64_ALL = "DEM_4.28"
POP_65_74_ALL = "DEM_4.31"
POP_75_PLUS_ALL = "DEM_4.34"


def sample_vs_population(path_to_sample: str, path_to_population: str, path_to_output: str):
    pop = read_population(path_to_population)
    sample = read_sample(path_to_sample)

    total_pop = pop[POP_ALL].sum()
    total_sample = sample.lfdn.count()

    table = pd.DataFrame(
        index=pd.MultiIndex.from_tuples([
            ("Gender", 'Male'),
            ("", 'Female'),
            ("Age", 'below 30 years'),
            ("", '30 - 39 years'),
            ("", '40 - 49 years'),
            ("", '50 - 59 years'),
            ("", 'older than 60 years'),
        ]),
        data={
            "sample": [
                sample[sample["gen"] == "male"].lfdn.count() / total_sample,
                sample[sample["gen"] == "feamle"].lfdn.count() / total_sample,
                sample[sample["age"] == "<30"].lfdn.count() / total_sample,
                sample[sample["age"] == "30-39"].lfdn.count() / total_sample,
                sample[sample["age"] == "40-49"].lfdn.count() / total_sample,
                sample[sample["age"] == "50-59"].lfdn.count() / total_sample,
                sample[sample["age"] == "60+"].lfdn.count() / total_sample,
            ],
            "population": [
                pop[POP_ALL_MALE].sum() / total_pop,
                pop[POP_ALL_FEMALE].sum() / total_pop,
                pop.loc[:, [POP_18_24_ALL, POP_25_29_ALL]].sum().sum() / total_pop,
                pop[POP_30_39_ALL].sum() / total_pop,
                pop[POP_40_49_ALL].sum() / total_pop,
                pop[POP_50_64_ALL].sum() / total_pop,
                pop.loc[:, [POP_65_74_ALL, POP_75_PLUS_ALL]].sum().sum() / total_pop,
            ]
        }
    )
    table.mul(100).to_csv(path_to_output, index=True, header=True, float_format="%.1f%%")


def read_population(path_to_data: str) -> pd.DataFrame:
    with open(path_to_data, encoding='unicode_escape') as file:
        pop_lines = [re.sub(r"\((\d+)\)", r"\g<1>", line) for line in file]
    pop = pd.read_csv(StringIO('\n'.join(pop_lines)), delimiter=";", na_values="-")
    return pop[pop["Reg_Hier"] == "Gemeinde"] # filter to municipalities


def read_sample(path_to_data: str) -> pd.DataFrame:
    sample = pd.read_feather(path_to_data)
    return sample


if __name__ == "__main__":
    sample_vs_population(
        path_to_sample=snakemake.input.sample,
        path_to_population=snakemake.input.population,
        path_to_output=snakemake.output[0]
    )
