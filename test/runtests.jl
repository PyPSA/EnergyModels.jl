import EnergyModels
const EM = EnergyModels

@static if VERSION < v"0.7.0-DEV.2005"
    using Base.Test
else
    using Test
end

@testset "EnergyModels" begin
    # @test isempty(detect_ambiguities(EM, Base, Core))

    @testset "IO" begin
        include("io.jl")
    end
end
