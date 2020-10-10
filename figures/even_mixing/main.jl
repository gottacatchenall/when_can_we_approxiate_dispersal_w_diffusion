using MetapopulationDynamics
using CSV

param_dict = Dict(
        "migration_probability" => [i for i in 0.01:0.01:1],
        "lambda" => [7],
        "model" => [RickerModelWDiffusionDispersal()],
        "alpha" => [0.0, 3.0, 6.0, 9.0],
        "number_of_populations" => [i for i in 2:25],
        "predation_strength" => [0.03],
        "reproduction_probability" => [1.0],
        "number_of_replicates" => [100],
        
)

treatments = create_treatments_from_param_dictionary(param_dict, 
                                                    summary_stat=[PCC(), MeanAbundance()])

run_treatments(treatments)


CSV.write("metadata.csv", treatments.metadata)
CSV.write("output.csv", treatments.output)
