mod Later {

    pub enum ImpureLazy[ef: Eff, a: Type](Ref[ImpureLazy.Value[ef, a], Static])

    mod ImpureLazy {

        // shouldn't really be pub
        pub enum Value[ef: Eff, a: Type] {
            case Todo(Unit -> a \ ef)
            case Done(a)
        }

        pub def upcast(l: ImpureLazy[ef1, a]): ImpureLazy[ef1 + ef2, a] = unchecked_cast(l as ImpureLazy[ef1 + ef2, a])

        /// Pulls the value out of the ImpureLazy.
        def extract(l: ImpureLazy[ef, a]): Value[ef, a] \ IO = {
            let ImpureLazy(valRef) = l;
            deref valRef
        }

        /// Sets the value in the ImpureLazy.
        def inject_(x: a, l: ImpureLazy[ef, a]): Unit \ IO = {
            let ImpureLazy(valRef) = l;
            valRef := Value.Done(x)
        }
        
        /// Creates an ImpureLazy from a constant.
        pub def fromConstant(x: a): ImpureLazy[{}, a] = {
            ImpureLazy(unchecked_cast(ref Value.Done(x) @ Static as _ \ {}))
        }

        /// Creates an ImpureLazy from a function.
        pub def fromFunction(f: Unit -> a \ ef): ImpureLazy[ef, a] = {
            ImpureLazy(unchecked_cast(ref Value.Todo(f) @ Static as _ \ {}))
        }

        def internalGet(l: ImpureLazy[ef, a]): a \ IO + ef = match extract(l) {
            case Value.Todo(f) => 
                let newVal = f();
                inject_(newVal, l);
                newVal

            case Value.Done(x) => x
        }

        pub def get(l: ImpureLazy[ef, a]): a \ ef = unchecked_cast(internalGet(l) as _ \ ef)

        pub def map(f: a -> b \ efF, l: ImpureLazy[efL, a]): ImpureLazy[efF + efL, b] = {
            fromFunction(() -> f(get(l)))
        }

        pub def eq(l1: ImpureLazy[ef1, a], l2: ImpureLazy[ef2, a]): Bool \ {ef1, ef2} with Eq[a] = {
            get(l1) == get(l2)
        }
    }
}