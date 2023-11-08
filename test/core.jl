include("../src/SyntacticModels.jl")

using .SyntacticModels
using .SyntacticModels.ASKEMDecapodes
using .SyntacticModels.ASKEMCollages

using Test
using JSON3

jsondir = joinpath(@__DIR__, "json")

write_json_model(m, prefix=joinpath(@__DIR__, "json")) = open(joinpath(prefix, "$(m.header.name).json"), "w") do fp
  JSON3.pretty(fp, Dict(m))
end

readback(m, prefix=joinpath(@__DIR__, "json")) = open(joinpath(jsondir, "$(m.header.name).json"), "r") do fp
  JSON3.read(fp, typeof(m))
end

write_json_model(m::ASKEMDecapodes.ASKEMDecapode, prefix=joinpath(@__DIR__, "json")) = open(joinpath(prefix, "$(m.header.name).json"), "w") do fp
  d = Dict("header"=>m.header, "model"=>generate_json_acset(m.model), "_type"=> "ASKEMDecapode")
  JSON3.pretty(fp, d)
end

write_json_model(m::ASKEMCollages.ASKEMCollageIC, prefix=joinpath(@__DIR__, "json")) = open(joinpath(prefix, "$(m.header.name).json"), "w") do fp
  d = Dict("header"=>m.header, "morphism"=>generate_json_acset_transformation(m.model.morphism), "_type"=> "ASKEMCollageIC")
  JSON3.pretty(fp, d)
end
write_json_model(m::ASKEMCollages.ASKEMCollageBC, prefix=joinpath(@__DIR__, "json")) = open(joinpath(prefix, "$(m.header.name).json"), "w") do fp
  d = Dict("header"=>m.header, "morphism"=>generate_json_acset_transformation(m.model.morphism), "_type"=> "ASKEMCollageBC")
  JSON3.pretty(fp, d)
end
write_json_model(m::ASKEMCollages.ASKEMCollage, prefix=joinpath(@__DIR__, "json")) = open(joinpath(prefix, "$(m.header.name).json"), "w") do fp
  d = Dict("header"=>m.header, "collage"=>["bc"=>generate_json_acset_transformation(m.model.bc.morphism), "ic"=>generate_json_acset_transformation(m.model.ic.morphism)], "_type"=> "ASKEMCollage")
  JSON3.pretty(fp, d)
end


sm_write_json_acset(X, fname, prefix=joinpath(@__DIR__, "json")) = open(joinpath(prefix, "$(fname).json"), "w") do fp
  JSON3.pretty(fp, generate_json_acset(X))
end

try
  mkdir(joinpath(@__DIR__, "json"))
catch
  @info "JSON DIR already exists, you might want to rm -r it to clean up"
end

include("amr_examples.jl")
include("decapodes_examples.jl")
include("uwd_examples.jl")
# include("composite_models_examples.jl")
include("serialization_mwe.jl")
include("collages_examples.jl")
include("composite_bounded_examples.jl")
