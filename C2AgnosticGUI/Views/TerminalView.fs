module SysAdminManagementApp.TerminalView

open Elmish
open Elmish.Sub
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Input
open Avalonia
open Avalonia.Media
open Avalonia.Layout
open Avalonia.Controls.Primitives
open System.Diagnostics
open System.IO
open System

open SysAdminManagementApp.Models

let private isNullOrWhiteSpace (s: string) =
    match s with
    | null -> true
    | s when s.Trim().Length = 0 -> true
    | _ -> false


let startProcess (terminalId: int) (commandId: int) (command: string) (currentPath: string) dispatch =
    async {
        let processInfo = new ProcessStartInfo()
        processInfo.FileName <- if Environment.OSVersion.Platform = PlatformID.Win32NT then "cmd.exe" else "bash"
        processInfo.Arguments <- if Environment.OSVersion.Platform = PlatformID.Win32NT then $"/c {command}" else $"-c \"{command}\""
        processInfo.RedirectStandardOutput <- true
        processInfo.RedirectStandardError <- true
        processInfo.RedirectStandardInput <- true
        processInfo.UseShellExecute <- false
        processInfo.CreateNoWindow <- true
        processInfo.WorkingDirectory <- currentPath

        let _process = new Process()
        _process.StartInfo <- processInfo

        _process.OutputDataReceived.Add(fun args -> if isNull(args.Data) |> not then dispatch (OnTerminalMsg (terminalId, TerminalMsg.CommandOutput (commandId, args.Data))))
        _process.ErrorDataReceived.Add(fun args -> if isNull(args.Data) |> not then dispatch (OnTerminalMsg (terminalId, TerminalMsg.CommandOutput (commandId, args.Data))))

        _process.Start() |> ignore

        let streamWriter = _process.StandardInput

        _process.BeginOutputReadLine()
        _process.BeginErrorReadLine()

        streamWriter.Close()

        _process.WaitForExit() |> ignore
    }
    |> Async.StartImmediate

let changeDirectory (terminalId: int) (commandId: int) (path: string) (currentPath: string) dispatch =
    let newPath =
        if Path.IsPathRooted(path) then path
        else Path.GetFullPath(Path.Combine(currentPath, path))
    if Directory.Exists(newPath) then
        dispatch (OnTerminalMsg (terminalId, TerminalMsg.SetCurrentPath newPath))
    else
        dispatch (OnTerminalMsg (terminalId, TerminalMsg.CommandOutput (commandId, $"Directory '{newPath}' does not exist.")))


let onTerminalRenameMsg (terminalId: int) (model: StateModel) (msg: TerminalRenameMsg) =
    match msg with
    | StartRenaming ->
        let updatedTerminals =
            model.Terminals
            |> List.map
                (fun terminal ->
                    if terminal.Id = terminalId then
                        {terminal with IsRenaming = true }
                    else
                        terminal
                )
        { model with Terminals = updatedTerminals }

    | NameTextChanged text ->
        let updatedTerminals =
            model.Terminals
            |> List.map
                (fun terminal ->
                    if terminal.Id = terminalId then
                        {terminal with RenamingText = text }
                    else
                        terminal
                )
        { model with Terminals = updatedTerminals }

    | Rename ->
        let updatedTerminals =
            model.Terminals
            |> List.map
                (fun terminal ->
                    if terminal.Id = terminalId then
                        {terminal with
                            Name = terminal.RenamingText
                            IsRenaming = false
                        }
                    else
                        terminal
                )
        { model with Terminals = updatedTerminals }

let updateTerminalInList (terminalId: int) (existingTermials: List<Terminal>) (updater: Terminal -> Terminal) =
    existingTermials
    |> List.map
        (fun terminal ->
            if terminal.Id = terminalId then
                updater terminal
            else
                terminal
        )

let onTerminalMsg (terminalId: int) (model: StateModel) (msg: TerminalMsg) =
    match msg with
    | CommandChanged command ->
        let updatedTerminals =
            updateTerminalInList
                terminalId
                model.Terminals
                (fun terminal -> { terminal with Command = command })

        { model with Terminals = updatedTerminals }, Cmd.none

    | RunCommand ->
        let terminal = model.Terminals |> List.find (fun terminal -> terminal.Id = terminalId)
        let commandId = terminal.History.Length + 1

        if terminal.Command.StartsWith("cd ") then
            let path = terminal.Command.Substring(3)

            let updatedTerminals =
                updateTerminalInList
                    terminalId
                    model.Terminals
                    (fun terminal ->
                        { terminal with
                            Command = String.Empty
                            History =
                                [
                                    {
                                        Id      = commandId
                                        Command = terminal.Command
                                        Path    = terminal.CurrentPath
                                        Output  = String.Empty
                                    }
                                ]
                                |> List.append terminal.History
                            HistoryIndex = terminal.History.Length
                        }
                    )

            { model with Terminals = updatedTerminals }, Cmd.ofEffect (fun dispatch -> changeDirectory terminalId commandId path terminal.CurrentPath dispatch)

        else if isNullOrWhiteSpace terminal.Command then
            model, Cmd.none

        else
            let updatedTerminals =
                updateTerminalInList
                    terminalId
                    model.Terminals
                    (fun terminal ->

                        { terminal with
                            History =
                                [
                                    {
                                        Id      = terminal.History.Length + 1
                                        Command = terminal.Command
                                        Path    = terminal.CurrentPath
                                        Output  = String.Empty
                                    }
                                ]
                                |> List.append terminal.History

                            HistoryIndex = terminal.History.Length
                            Command      = String.Empty
                        }
                    )

            { model with Terminals = updatedTerminals }, Cmd.ofEffect (fun dispatch -> startProcess terminalId commandId terminal.Command terminal.CurrentPath dispatch)


    | CommandOutput (commandId, output) ->
        let updatedTerminals =
            updateTerminalInList
                terminalId
                model.Terminals
                (fun terminal ->
                    let updatedHistory =
                        terminal.History
                        |> List.map
                            (fun trace ->
                                if trace.Id = commandId then
                                    { trace with Output = trace.Output + $"{output} \n" }
                                else
                                    trace
                            )

                    { terminal with History = updatedHistory }
                )

        { model with Terminals = updatedTerminals }, Cmd.none
    | PreviousCommand ->
        let updatedTerminals =
            updateTerminalInList
                terminalId
                model.Terminals
                (fun terminal ->
                    let newIndex =
                        if terminal.HistoryIndex + 1 < terminal.History.Length then
                            terminal.HistoryIndex + 1
                        else
                            terminal.HistoryIndex

                    let newCommand =
                        if newIndex >= 0 then
                            terminal.History |> List.tryItem newIndex |> Option.map (fun trace -> trace.Command) |> Option.defaultValue String.Empty
                        else
                            terminal.Command

                    { terminal with
                        HistoryIndex = newIndex
                        Command      = newCommand
                    }
                )

        { model with Terminals = updatedTerminals }, Cmd.none
    | NextCommand ->
        let updatedTerminals =
            updateTerminalInList
                terminalId
                model.Terminals
                (fun terminal ->
                    let newIndex =
                        if terminal.HistoryIndex - 1 >= 0 then
                            terminal.HistoryIndex - 1
                        else
                            terminal.HistoryIndex

                    let newCommand =
                        if newIndex >= 0 then
                            terminal.History |> List.tryItem newIndex |> Option.map (fun trace -> trace.Command) |> Option.defaultValue String.Empty
                        else
                            terminal.Command

                    { terminal with
                        HistoryIndex = newIndex
                        Command      = newCommand
                    }
                )

        { model with Terminals = updatedTerminals }, Cmd.none
    | SetCurrentPath path ->
        let updatedTerminals =
            updateTerminalInList
                terminalId
                model.Terminals
                (fun terminal -> { terminal with CurrentPath = path })

        { model with Terminals = updatedTerminals }, Cmd.none
    | SelectTerminal ->
        { model with SelectedTerminalId = terminalId }, Cmd.none
    | RemoveTerminal ->
        let updatedTerminals = model.Terminals |> List.filter (fun terminal -> terminal.Id <> terminalId)
        { model with
            Terminals          = updatedTerminals
            SelectedTerminalId = updatedTerminals |> List.tryLast |> Option.map (fun terminal -> terminal.Id) |> Option.defaultValue 0
        }, Cmd.none

    | OnTerminalRenameMsg msg ->
        onTerminalRenameMsg terminalId model msg, Cmd.none


let view (terminal: Terminal) (dispatch: Msg -> unit) : Types.IView =
    Border.create [
        //Border.background "Black"
        Border.verticalAlignment VerticalAlignment.Stretch
        Border.horizontalAlignment HorizontalAlignment.Stretch
        Border.borderThickness (Thickness 0.0)
        Border.margin 0
        Border.padding (5,5,5,5)

        Border.child (
            ScrollViewer.create [
                ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto
                ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Disabled
                ScrollViewer.horizontalAlignment HorizontalAlignment.Stretch

                ScrollViewer.content (
                    StackPanel.create [
                        StackPanel.orientation Orientation.Vertical

                        StackPanel.children (
                                let history: Types.IView<StackPanel> list =
                                    terminal.History
                                    |> List.map
                                        (fun commandTrace ->
                                            StackPanel.create [
                                                StackPanel.children [
                                                    TextBlock.create [
                                                        TextBlock.horizontalAlignment HorizontalAlignment.Left
                                                        TextBlock.text $"{commandTrace.Path}> {commandTrace.Command}"
                                                        TextBlock.focusable false
                                                        TextBlock.isEnabled false
                                                        TextBlock.multiline true
                                                        TextBlock.textAlignment TextAlignment.Left
                                                        TextBlock.horizontalAlignment HorizontalAlignment.Stretch
                                                        TextBlock.textWrapping TextWrapping.Wrap
                                                    ]

                                                    TextBlock.create [
                                                        TextBlock.text commandTrace.Output
                                                        TextBlock.horizontalAlignment HorizontalAlignment.Stretch
                                                        TextBlock.textWrapping TextWrapping.Wrap
                                                        TextBlock.focusable false
                                                        TextBlock.multiline true
                                                        TextBlock.isVisible ((isNullOrWhiteSpace commandTrace.Output) |> not)
                                                    ]
                                                ]
                                            ]
                                        )

                                let commandText: Types.IView<StackPanel> =
                                    StackPanel.create [
                                        StackPanel.children [
                                            Grid.create [
                                                Grid.columnDefinitions "Auto, *"

                                                Grid.children [
                                                    TextBlock.create [
                                                        Grid.column 0
                                                        TextBlock.text $"{terminal.CurrentPath}> "
                                                        TextBlock.focusable false
                                                        TextBlock.textAlignment TextAlignment.Left
                                                        TextBlock.verticalAlignment VerticalAlignment.Center
                                                        TextBlock.horizontalAlignment HorizontalAlignment.Left
                                                        TextBlock.multiline true
                                                        TextBlock.textWrapping TextWrapping.Wrap
                                                        TextBlock.isEnabled false
                                                    ]

                                                    let textbox =
                                                        TextBox.create [
                                                            Grid.column 1
                                                            TextBox.text terminal.Command
                                                            TextBox.borderThickness 0
                                                            TextBox.cornerRadius 0
                                                            TextBox.textWrapping TextWrapping.Wrap
                                                            TextBox.borderBrush (SolidColorBrush(Colors.Transparent))
                                                            TextBox.acceptsTab false

                                                            TextBox.verticalAlignment VerticalAlignment.Center
                                                            TextBox.verticalContentAlignment VerticalAlignment.Center
                                                            TextBox.horizontalContentAlignment HorizontalAlignment.Stretch
                                                            TextBox.horizontalAlignment HorizontalAlignment.Stretch

                                                            TextBox.onPointerEntered (fun args ->
                                                                let textbox = args.Source :?> TextBox
                                                                if textbox.Classes.Contains(":focus") then
                                                                    textbox.BorderBrush <- SolidColorBrush(Colors.Transparent)
                                                                else
                                                                    textbox.Focus() |> ignore
                                                                args.Handled <- true
                                                            )

                                                            TextBox.onTextChanged (fun text -> dispatch (OnTerminalMsg (terminal.Id, TerminalMsg.CommandChanged text)))
                                                            TextBox.onKeyDown (fun args ->
                                                                match args.Key with
                                                                | Key.Enter -> dispatch (OnTerminalMsg (terminal.Id, TerminalMsg.RunCommand))
                                                                | Key.Up    -> dispatch (OnTerminalMsg (terminal.Id, TerminalMsg.PreviousCommand))
                                                                | Key.Down  -> dispatch (OnTerminalMsg (terminal.Id, TerminalMsg.NextCommand))
                                                                | _ -> ())
                                                        ]

                                                    textbox
                                                ]
                                            ]
                                        ]
                                    ]

                                List.append history [commandText]
                                |> List.map (fun x -> x :> Types.IView)
                        )

                    ]
                )
            ]
        )
    ]
