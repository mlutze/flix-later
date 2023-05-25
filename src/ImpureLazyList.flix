mod Later {

    pub enum ImpureLazyList[ef: Eff, a: Type](ImpureLazy[ef, ImpureLazyList.Node[ef, a]])

    mod ImpureLazyList {

        pub def upcast(l: ImpureLazyList[ef1, a]): ImpureLazyList[ef1 + ef2, a] = unchecked_cast(l as ImpureLazyList[ef1 + ef2, a])

        pub enum Node[ef: Eff, a: Type] {
            case ImpureNil
            case ImpureCons(a, ImpureLazyList[ef, a])
        }

        def getValue(l: ImpureLazyList[ef, a]): Node[ef, a] \ ef = {
            let ImpureLazyList(x) = l;
            Later.ImpureLazy.get(x)
        }

        pub def fromIterator(it: Iterator[a, ef, rI]): ImpureLazyList[ef + rI, a] = {
            def getList() = match Iterator.next(it) {
                case None => Node.ImpureNil
                case Some(x) => Node.ImpureCons(x, fromIterator(it))
            };
            ImpureLazyList(Later.ImpureLazy.fromFunction(getList))
        }

        pub def toIterator(rc: Region[r], l: ImpureLazyList[ef, a]): Iterator[a, ef, r] \ r = {
            let lRef = ref l @ rc; 
            def next() = match uncons(l) {
                case None => Iterator.Step.Done
                case Some((hd, tl)) => {
                    lRef := tl;
                    Iterator.Step.Ans(hd)
                }
            };
            Iterator.Iterator(rc, next)
        }

        pub def map(f: a -> b \ efF, l: ImpureLazyList[efL, a]): ImpureLazyList[efF + efL, b] = {
            def getList() = match getValue(l) {
                case Node.ImpureNil => Node.ImpureNil
                case Node.ImpureCons(x, xs) => Node.ImpureCons(f(x), map(f, xs))
            };
            ImpureLazyList(Later.ImpureLazy.fromFunction(getList))
        }

        pub def filter(f: a -> Bool \ efF, l: ImpureLazyList[efL, a]): ImpureLazyList[efF + efL, a] = {
            def getList() = match getValue(l) {
                case Node.ImpureNil => Node.ImpureNil
                case Node.ImpureCons(x, xs) =>
                    if (f(x)) {
                        Node.ImpureCons(x, filter(f, xs))
                    } else {
                        getValue(filter(f, xs))
                    }
            };
            ImpureLazyList(Later.ImpureLazy.fromFunction(getList))
        }

        pub def head(l: ImpureLazyList[ef, a]): Option[a] \ ef = {
            match getValue(l) {
                case Node.ImpureNil => None
                case Node.ImpureCons(x, _) => Some(x)
            }
        }


        pub def tail(l: ImpureLazyList[ef, a]): Option[ImpureLazyList[ef, a]] \ ef = {
            match getValue(l) {
                case Node.ImpureNil => None
                case Node.ImpureCons(_, xs) => Some(xs)
            }
        }

        pub def uncons(l: ImpureLazyList[ef, a]): Option[(a, ImpureLazyList[ef, a])] \ ef = {
            match getValue(l) {
                case Node.ImpureNil => None
                case Node.ImpureCons(x, xs) => Some((x, xs))
            }
        }

        pub def cons(x: a, l: ImpureLazyList[ef, a]): ImpureLazyList[ef, a] = {
            def getList() = checked_ecast(Node.ImpureCons(x, l));
            ImpureLazyList(Later.ImpureLazy.fromFunction(getList))
        }

        pub def take(n: Int32, l: ImpureLazyList[ef, a]): List[a] \ ef = {
            def loop(acc, m, l1) = {
                if (m <= 0) {
                    acc
                } else {
                    match uncons(l1) {
                        case Some((x, xs)) => loop(x :: acc, m - 1, xs)
                        case None => acc
                    }   
                }
            };
            loop(Nil, n, l) |> List.reverse
        }
    }
}