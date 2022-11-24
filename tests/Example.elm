module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


dummyTest =
    describe "Dummy test is so dummy"
        [ test "The first case" (\_ -> 2 |> Expect.equal 2) ]
