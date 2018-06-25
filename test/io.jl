# Test I/O
module Mock
type MockGenerator <: Component
    model::EnergyModel
end

@testset "NetCDF" begin
    model = EnergyModel()
    MockGenerator(model)
    naming(model, :)
    defVar()

    io = EM.NcIO("test.nc")
    @test EM.get(io, :generator) == nothing
end
