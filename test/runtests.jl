import EnergyModels
const EM = EnergyModels

using Test

@testset "EnergyModels" begin
    # @test isempty(detect_ambiguities(EM, Base, Core))

    @testset "WrappedArray" begin
        include("wrappedarray.jl")
    end
end
