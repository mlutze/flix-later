mod Later.TestImpureLazy {
    use Later.ImpureLazy
    use Later.ImpureLazy.fromFunction
    use Later.ImpureLazy.fromConstant
    use Later.ImpureLazy.eq
    use Later.ImpureLazy.get
    use Later.ImpureLazy.map

    /// Returns true only once. Thereafter returns false.
    def getTrueOnceFunc(): (Unit -> Bool \ IO) \ IO = {
        let first = ref true @ Static;
        def f() = {
            let res = deref first;
            first := false;
            res
        };
        f
    }

    @Test
    def testGetTrueOnceFunc(): Bool \ IO = {
        let f = getTrueOnceFunc();
        List#{f(), f(), f(), f()} == List#{true, false, false, false}
    }

    @Test
    def testFromFunctionFromFunction(): Bool = {
        let l1 = fromFunction(() -> 1);
        let l2 = fromFunction(() -> 1);
        l1 `eq` l2
    }

    @Test
    def testEvalOnce01(): Bool \ IO = {
        let l1 = fromFunction(getTrueOnceFunc());
        get(l1) == get(l1)
    }

    @Test
    def testEvalOnce02(): Bool \ IO = {
        let l1 = fromFunction(getTrueOnceFunc());
        get(l1) == get(l1) and get(l1) == get(l1)
    }

    @Test
    def testMap(): Bool \ IO = {
        let l1 = fromFunction(getTrueOnceFunc());
        let v = l1 |> map(ToString.toString) |> get;
        v == "true"
    }

}