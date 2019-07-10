open Expecto
open Newtonsoft.Json.Linq
open ParseWorksheet
open System.IO

let parseWorksheetTests =
    testList "Parse worksheet" [
        testCase "Can parse sample data" (fun () ->
            let data =
                File.ReadAllText "Common.Test\\sample-data.json"
                |> JToken.Parse
                |> Worksheet.tryParse
            let expected =
                [
                    {
                        Student = { LastName = "Munoz"; FirstName = "Hilda" }
                        Performances = [
                            { Discipline = { Name = "8 min-Lauf"; Measurement = "Strecke (m)" }; Value = 1350.; Points = 625 }
                            { Discipline = { Name = "Stangenklettern"; Measurement = "Markier-ungen" }; Value = 10.; Points = 450 }
                            { Discipline = { Name = "Hürdenbumerang-lauf"; Measurement = "Zeit (s)" }; Value = 16.53; Points = 597 }
                            { Discipline = { Name = "Hochsprung"; Measurement = "Höhe (cm)" }; Value = 0.; Points = 0 }
                            { Discipline = { Name = "60 m-Sprint"; Measurement = "Zeit (s)" }; Value = 0.; Points = 0 }
                            { Discipline = { Name = "Kugelstossen       (6,25 kg)"; Measurement = "Weite (cm)" }; Value = 0.; Points = 0 }
                            { Discipline = { Name = "Weitsprung"; Measurement = "Weite (cm)" }; Value = 0.; Points = 0 }
                            { Discipline = { Name = "100 m-Schwimmen"; Measurement = "Zeit (s)" }; Value = 0.; Points = 0 }
                        ]
                    }
                ]
                |> Ok
            Expect.equal data expected "Parsing should succeed"
        )
    ]

[<EntryPoint>]
let main argv =
    runTests defaultConfig parseWorksheetTests
