using MetapopulationDynamics
using CSV

param_dict = Dict(
        "migration_probability" => [0.01, 0.1, 0.25, 0.5],
        "lambda" => [i for i in 2.0:0.25:14.0],
        "model" => [RickerModelWDiffusionDispersal(), RickerModelWStochasticDispersal()],
        "alpha" => [0.0],
        "number_of_populations" => [5, 10, 15, 20],
        "predation_strength" => [0.03],
        "reproduction_probability" => [1.0],
        "number_of_replicates" => [200],
        
)

treatments = create_treatments_from_param_dictionary(param_dict, 
                                                    summary_stat=[PCC(), MeanAbundance()])

run_treatments(treatments)


CSV.write("metadata.csv", treatments.metadata)
CSV.write("output.csv", treatments.output)
