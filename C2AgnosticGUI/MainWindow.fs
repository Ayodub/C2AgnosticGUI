[<AutoOpen>]
module SysAdminManagementApp.MainWindow

open Avalonia
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish

type MainWindow() as this =
    inherit HostWindow()
    do
        #if DEBUG
        this.AttachDevTools()
        #endif

        base.Title <- "SysAdmin Manager"

        Elmish.Program.mkProgram MainView.init MainView.update MainView.view
        |> Program.withHost this
        |> Program.withConsoleTrace
        |> Program.run