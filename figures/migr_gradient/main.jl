using MetapopulationDynamics
using CSV

param_dict = Dict(
        "migration_probability" => [i for i in 0:0.01:1],
        "lambda" => [2, 5, 8, 11, 14],
        "model" => [RickerModelWDiffusionDispersal(), RickerModelWStochasticDispersal()],
        "alpha" => [0.0, 3.0, 6.0, 9.0],
        "number_of_populations" => [10],
        "predation_strength" => [0.03],
        "reproduction_probability" => [1.0],
        "number_of_replicates" => [100],
        
)

treatments = create_treatments_from_param_dictionary(param_dict, 
                                                    summary_stat=[PCC(), MeanAbundance()])

run_treatments(treatments)


CSV.write("metadata.csv", treatments.metadata)
CSV.write("output.csv", treatments.output)
