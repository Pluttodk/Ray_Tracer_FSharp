module Assert

type Assert() = 
    static let mutable Passed = 0
    static let mutable Failed = 0
    static member Equal (expected,result,name) = 
        if (expected = result) then 
            Passed <- Passed + 1
            printfn "--------Test Passed! %s" name
        else 
            Failed <- Failed + 1
            printfn "--------TEST FAILED!!! Expected (%A) Actual (%A) in %s" expected result name
    static member True (value,name) =
        if value then 
            Passed <- Passed + 1
            printfn "--------Test Passed! %s" name
        else 
            Failed <- Failed + 1
            printfn "--------TEST FAILED!!! Assert true is false (%A) in %s" value name
    static member EqualMany testlist f =
        for (name, input, expected) in testlist do
              let result = f input
              if (expected = result) then 
                  Passed <- Passed + 1
                  printfn "--------Test Passed! %s" name
              else 
                  Failed <- Failed + 1
                  printfn "--------TEST FAILED!!! Expected (%A) Actual (%A) in %s" expected result name
    static member AmountPassed = Passed
    static member AmountFailed = Failed
    static member Fail name =
        Failed <- Failed + 1
        eprintfn "--------TEST FAILED!!! %s" name
    static member Reset() =
        Passed <- 0
        Failed <- 0
    static member Throws<'T when 'T :> exn> (action: unit -> unit, name: string) =
        try
            action ()
            Assert.Fail(sprintf "%s: expected %s" name typeof<'T>.Name)
        with
        | :? 'T -> Assert.True(true, name)
        | error -> Assert.Fail(sprintf "%s: expected %s, received %O" name typeof<'T>.Name error)