using AxisArrays

@testset "With AxisArray" begin
    a = AxisArray(reshape(1:6, 3, 2), Axis{:foo}(2:4), Axis{:bar}(3:4))
    # 2-dimensional AxisArray{Int64,2,...} with axes:
    #     :foo, 2:4
    #     :bar, 3:4
    # And data, a 3×2 Base.ReshapedArray{Int64,2,UnitRange{Int64},Tuple{}}:
    #  1  4
    #  2  5
    #  3  6

    v = Axis{:virt}(5:8)

    w = @inferred(EM.WrappedArray(a, (v, AxisArrays.axes(a)...)))

    for i = (5, 10, :a)
        @test @inferred(w[i, :, :]) == a
    end

    w2 = EM.WrappedArray(a, (AxisArrays.axes(a, 2), v, AxisArrays.axes(a, 1)))
    @test isa(@inferred(w2[1, 4, 3]), Number)
    @test w2[1, 4, 3] == a[3, 1]
end

@testset "With constant" begin
    using AxisArrays

    b = 2.0
    v = Axis{:virt}(5:8)

    w3 = EM.WrappedArray(b, v)
    @test @inferred(w3[5]) == b
    @test @inferred(w3[8]) == b
end
