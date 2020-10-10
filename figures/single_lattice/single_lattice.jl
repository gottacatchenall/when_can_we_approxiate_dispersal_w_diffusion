using MetapopulationDynamics
using CSV

param_dict = Dict(
        "migration_probability" => [i for i in 0.01:0.02:1],
        "lambda" => [i for i in 2.0:0.25:14.0],
        "model" => [RickerModelWDiffusionDispersal(), RickerModelWStochasticDispersal()],
        "alpha" => [0.0],
        "predation_strength" => [0.03],
        "reproduction_probability" => [1.0],
        "number_of_replicates" => [200]
)

treatments = create_treatments_from_param_dictionary(param_dict)

run_treatments(treatments)


CSV.write("metadata.csv", treatments.metadata)
CSV.write("output.csv", treatments.output)
