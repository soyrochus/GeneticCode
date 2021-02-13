namespace GenCodeLib
module Say =
    let hello name =
        $"Hello %s{name}"


    module Test = 
        open NUnit.Framework
        open FsUnit

        [<Test>]
        let ``Say hello``() = 
            hello "you!"  |> should equal "Hello you!"