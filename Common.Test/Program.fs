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
                            { Discipline = { Name = "8 min-Lauf"; Measurement = "Strecke (m)" }; MeasurementValue = Some 1350.; Points = Some 625 }
                            { Discipline = { Name = "Stangenklettern"; Measurement = "Markier-ungen" }; MeasurementValue = Some 10.; Points = Some 450 }
                            { Discipline = { Name = "Hürdenbumerang-lauf"; Measurement = "Zeit (s)" }; MeasurementValue = Some 16.53; Points = Some 597 }
                            { Discipline = { Name = "Hochsprung"; Measurement = "Höhe (cm)" }; MeasurementValue = None; Points = None }
                            { Discipline = { Name = "60 m-Sprint"; Measurement = "Zeit (s)" }; MeasurementValue = None; Points = None }
                            { Discipline = { Name = "Kugelstossen       (6,25 kg)"; Measurement = "Weite (cm)" }; MeasurementValue = None; Points = None }
                            { Discipline = { Name = "Weitsprung"; Measurement = "Weite (cm)" }; MeasurementValue = None; Points = None }
                            { Discipline = { Name = "100 m-Schwimmen"; Measurement = "Zeit (s)" }; MeasurementValue = None; Points = None }
                        ]
                    }
                ]
                |> Ok
            Expect.equal data expected "Sample data should be parsed correctly"
        )
    ]

[<EntryPoint>]
let main argv =
    runTests defaultConfig parseWorksheetTests
