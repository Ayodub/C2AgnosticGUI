module SysAdminManagementApp.MainView

open Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Platform.Storage
open Avalonia.Media
open Avalonia.FuncUI.Elmish
open Avalonia.Input
open Avalonia.Styling



let init () =
    {
        Terminals                      = [ Terminal.Default 1 ]
        SelectedTerminalId             = 1
        Devices                        = []
        View                           = ListView
        DeviceAcc                      = None
        SetDeviceFormVisible           = false
        DraggingDevice                 = None
        DragStart                      = Point(0.0, 0.0)
        DragOffset                     = Point(0.0, 0.0)
        Connections                    = Set.empty
        ConnectionStartDeviceId        = None
        PointerPositionOnCanvas        = Point(0.0, 0.0)
        ZoomLevel                      = 1.0
        SettingOsForDeviceId           = None
        VisibleOptionsForDevicId       = None
    },
    Cmd.none

let updateDeviceAcc (model: StateModel) (msg: SetDeviceInfoMsg) =
    match msg with
    | SetDeviceName name -> { model with DeviceAcc = Option.map (fun deviceAcc -> { deviceAcc with Name = name }) model.DeviceAcc }
    | SetDeviceIP ip -> { model with DeviceAcc = Option.map (fun deviceAcc -> { deviceAcc with IP = ip }) model.DeviceAcc }
    | SetAssetName assetName -> { model with DeviceAcc = Option.map (fun deviceAcc -> { deviceAcc with AssetName = assetName }) model.DeviceAcc }
    | SetOsName osName -> { model with DeviceAcc = Option.map (fun deviceAcc -> { deviceAcc with OsName = osName }) model.DeviceAcc }
    | SetDeviceOsType osType -> { model with DeviceAcc = Option.map (fun deviceAcc -> { deviceAcc with OsType = Some osType }) model.DeviceAcc }
    | SetDeviceOs (deviceId, osType) ->
        let updatedDevices = model.Devices |> List.map (fun d -> if d.Id = deviceId then {d with OsType = Some osType} else d)

        { model with Devices = updatedDevices }


let updateDraggingDevice (model: StateModel) (msg: DragMsg) =
    match msg with
    | DragStart (device, startPoint) ->
        let maybeDraggingDevice = model.Devices |> List.tryFind (fun d -> d.Id = device.Id)

        match maybeDraggingDevice with
        | None -> model
        | Some draggingDevice ->
            { model with
                DraggingDevice = Some draggingDevice
                DragStart = startPoint
                DragOffset = Point(startPoint.X - draggingDevice.Position.X, startPoint.Y - draggingDevice.Position.Y)
            }
    | Dragging point ->
        let newPosition = Point(point.X - model.DragOffset.X, point.Y - model.DragOffset.Y)

        match model.DraggingDevice with
        | Some device ->
            let updatedDevices = model.Devices |> List.map (fun d -> if d.Id = device.Id then {d with Position = newPosition} else d)

            { model with Devices = updatedDevices }
        | None ->
            model
    | DragEnd ->
        { model with DraggingDevice = None }

let manageConnection (model: StateModel) (msg: ConnectionMsg) =
    match msg with
    | SetConnection deviceId ->
        match model.ConnectionStartDeviceId with
        | Some startDeviceId when deviceId <> startDeviceId ->
            let connection: int * int = startDeviceId, deviceId

            { model with
                Connections             = model.Connections.Add connection
                ConnectionStartDeviceId = None
            }
        | None ->
            { model with ConnectionStartDeviceId = Some deviceId }
        | _ ->
            model

    | Disconnect (device1Id, device2Id) ->
        let updatedConnections =
            model.Connections
            |> Set.filter
                (fun connection ->
                    connection <> (device1Id, device2Id)
                )

        { model with Connections = updatedConnections }

let onAddDeviceMsg (model: StateModel) (msg: SetDeviceMsg) =
    match msg with
    | SetDevice ->
        { model with
            Devices =
                match model.Devices |> List.exists (fun d -> d.Id = model.DeviceAcc.Value.Id) with
                | true  -> model.Devices |> List.map (fun d -> if d.Id = model.DeviceAcc.Value.Id then model.DeviceAcc.Value else d)
                | false -> model.Devices @ [ model.DeviceAcc.Value ]
            DeviceAcc = None
            SetDeviceFormVisible = false
        }

    | ToggleSetDeviceForm maybeExistingDevice ->
        let deviceAcc =
            match model.SetDeviceFormVisible with
            | true  ->
                None
            | false ->
                if maybeExistingDevice.IsSome then
                    maybeExistingDevice
                else
                    Some {
                        Id        = model.Devices.Length + 1
                        Name      = ""
                        IP        = ""
                        AssetName = ""
                        OsName    = ""
                        Position  = Point(float (model.Devices.Length * (int model.ZoomLevel) * 100), model.ZoomLevel * 100.0)
                        OsType    = None
                    }

        { model with
            DeviceAcc            = deviceAcc
            SetDeviceFormVisible = not model.SetDeviceFormVisible
        }

    | OnSetDeviceInfoMsg addDeviceFormMsg ->
        updateDeviceAcc model addDeviceFormMsg

    | ToggleSetDeviceOsForm deviceId ->
        { model
            with
                SettingOsForDeviceId =
                    match model.SettingOsForDeviceId.IsSome with
                    | true  -> None
                    | false -> Some deviceId
        }

    | ToggleDeviceOptions deviceId ->
        { model
            with
                VisibleOptionsForDevicId =
                    match model.VisibleOptionsForDevicId.IsSome with
                    | true  -> None
                    | false -> Some deviceId
        }

let onZoomMsg (model: StateModel) (msg: ZoomMsg) =
    match msg with
    | ZoomIn ->
        { model with ZoomLevel = model.ZoomLevel * 1.1 }
    | ZoomOut ->
        { model with ZoomLevel = model.ZoomLevel / 1.1 }

let update msg model =
    match msg with
    | AddTerminal ->
        let terminal = Terminal.Default (model.Terminals.Length + 1)
        { model with
            Terminals          = model.Terminals @ [ terminal ]
            SelectedTerminalId = terminal.Id
        }, Cmd.none

    | OnSetDeviceMsg deviceAddMsg ->
        onAddDeviceMsg model deviceAddMsg, Cmd.none

    | SwitchView view ->
        { model with View = view }, Cmd.none

    | OnDragMsg dragMsg ->
        updateDraggingDevice model dragMsg, Cmd.none

    | OnConnectionMsg connectionMsg ->
        manageConnection model connectionMsg, Cmd.none

    | PointerMoved point ->
        { model with PointerPositionOnCanvas = point }, Cmd.none

    | OnTerminalMsg (terminalId, terminalMsg) ->
        TerminalView.onTerminalMsg terminalId model terminalMsg

    | OnZoomMsg zoomMsg ->
        onZoomMsg model zoomMsg, Cmd.none


let addDeviceForm (deviceAcc: Device) dispatch =
    StackPanel.create [
        StackPanel.spacing 3.0
        StackPanel.children [

            TextBox.create [
                TextBox.text deviceAcc.Name
                TextBox.watermark "Device Name"
                TextBox.onTextChanged (fun text -> dispatch (OnSetDeviceMsg (OnSetDeviceInfoMsg (SetDeviceName text))))
            ]
            TextBox.create [
                TextBox.text deviceAcc.IP
                TextBox.watermark "Device IP"
                TextBox.onTextChanged (fun text -> dispatch (OnSetDeviceMsg (OnSetDeviceInfoMsg (SetDeviceIP text))))
            ]
            TextBox.create [
                TextBox.text deviceAcc.AssetName
                TextBox.watermark "Asset Name"
                TextBox.onTextChanged (fun text -> dispatch (OnSetDeviceMsg (OnSetDeviceInfoMsg (SetAssetName text))))
            ]
            TextBox.create [
                TextBox.text deviceAcc.OsName
                TextBox.watermark "OS Name"
                TextBox.onTextChanged (fun text -> dispatch (OnSetDeviceMsg (OnSetDeviceInfoMsg (SetOsName text))))
            ]

            DockPanel.create [
                DockPanel.dock Dock.Left
                DockPanel.verticalAlignment VerticalAlignment.Top
                DockPanel.horizontalAlignment HorizontalAlignment.Left
                DockPanel.children [
                    TextBlock.create [
                        TextBlock.text "Choose Os Type:"
                    ]

                    yield!
                        OsType.GeAllCases()
                        |> List.map (fun osType ->
                            DockPanel.create [
                                DockPanel.dock Dock.Left
                                DockPanel.verticalAlignment VerticalAlignment.Top
                                DockPanel.horizontalAlignment HorizontalAlignment.Left
                                DockPanel.children [
                                    Image.create [
                                        Image.source osType.Logo
                                        Image.margin 2.0
                                        Image.width 30.0
                                        Image.height 30.0
                                        Image.onTapped (fun _ -> dispatch (OnSetDeviceMsg (OnSetDeviceInfoMsg (SetDeviceOsType osType))))
                                    ]
                                ]

                                DockPanel.background (
                                    match deviceAcc.OsType with
                                    | Some os when os = osType -> Brushes.Green
                                    | _ -> Brushes.Gray
                                )
                            ]
                            :> Types.IView
                        )
                ]
            ]


            Button.create [
                Button.content "Submit"
                Button.onClick (fun _ -> dispatch (OnSetDeviceMsg SetDevice))
            ]
        ]
    ]

let terminalTabView (selectedTerminalId: int) (terminals: List<Terminal>) dispatch =
    TabControl.create [
        TabControl.padding (0, 0, 0, 0)
        TabControl.viewItems (
            seq [
                yield!
                    terminals
                    |> List.mapi
                        (fun index (terminal) ->
                            TabItem.create [
                                TabItem.padding (2, 0, 0, 0)
                                TabItem.header (
                                    Border.create [
                                        Border.borderThickness 3.0
                                        Border.borderBrush "Gray"
                                        Border.padding 4
                                        Border.margin 0
                                        Border.cornerRadius (5,5,0,0)

                                        Border.child(
                                            Grid.create [
                                                Grid.columnDefinitions "Auto,Auto"

                                                Grid.children [

                                                    if terminal.IsRenaming then
                                                        TextBox.create [
                                                            Grid.column 0
                                                            TextBox.fontSize 16.0
                                                            TextBox.padding 0
                                                            TextBox.minHeight 16.0
                                                            TextBox.text terminal.Name
                                                            TextBox.tip "Press Enter or click outside to save"
                                                            TextBox.onTextChanged (fun text -> dispatch (OnTerminalMsg (terminal.Id, OnTerminalRenameMsg (NameTextChanged text))))
                                                            TextBox.onLostFocus (fun _ -> dispatch (OnTerminalMsg (terminal.Id, OnTerminalRenameMsg Rename)))
                                                            TextBox.onKeyDown (fun e ->
                                                                match e.Key with
                                                                | Key.Enter -> dispatch (OnTerminalMsg (terminal.Id, OnTerminalRenameMsg Rename))
                                                                | _ -> ()
                                                            )
                                                        ]
                                                    else
                                                        TextBlock.create [
                                                            Grid.column 0
                                                            TextBlock.margin (0,2,5,0)
                                                            TextBlock.fontSize 16.0
                                                            TextBlock.fontFamily "Consolas"
                                                            TextBlock.text terminal.Name
                                                            TextBlock.onTapped (fun _ -> dispatch (OnTerminalMsg (terminal.Id, SelectTerminal)))
                                                            TextBlock.onDoubleTapped (fun _ -> dispatch (OnTerminalMsg (terminal.Id, OnTerminalRenameMsg StartRenaming)))
                                                            TextBlock.tip "Double click to rename"
                                                        ]

                                                    Button.create [
                                                        Grid.column 1
                                                        Button.horizontalContentAlignment HorizontalAlignment.Right
                                                        Button.verticalContentAlignment VerticalAlignment.Center
                                                        Button.verticalAlignment VerticalAlignment.Center
                                                        Button.horizontalAlignment HorizontalAlignment.Right
                                                        Button.padding 0
                                                        Button.margin 0
                                                        Button.content (
                                                            Viewbox.create [
                                                                Viewbox.verticalAlignment VerticalAlignment.Center
                                                                Viewbox.width 10.0
                                                                Viewbox.child closeIconPath
                                                            ]
                                                        )
                                                        Button.tip "Close"
                                                        Button.background "Transparent"
                                                        Button.onClick (fun _ -> dispatch (OnTerminalMsg (terminal.Id, RemoveTerminal)))
                                                    ]
                                                ]
                                            ]
                                            :> Types.IView
                                        )

                                    ]
                                )

                                let isSelected = terminal.Id = selectedTerminalId

                                TabItem.isSelected isSelected
                                TabItem.content (TerminalView.view terminal dispatch)
                            ]
                            :> Types.IView
                        )

                yield
                    TabItem.create [
                        TabItem.padding (5, 0, 0, 0)
                        TabItem.header plusIconPath
                        TabItem.onTapped (fun _ -> dispatch AddTerminal)
                        TabItem.tip "Add New Terminal"
                    ]
                    :> Types.IView

            ]
            |> Seq.toList
        )
    ]


let view (model: StateModel) (dispatch) =
    DockPanel.create [
        DockPanel.children [
            Menu.create [
                Menu.dock Dock.Top
                Menu.viewItems [
                    MenuItem.create [
                        MenuItem.header "File"
                    ]
                    MenuItem.create [
                        MenuItem.header "View"
                        MenuItem.viewItems [
                            MenuItem.create [
                                MenuItem.header "List View"
                                MenuItem.onClick (fun _ -> dispatch (SwitchView ListView))
                            ]
                            MenuItem.create [
                                MenuItem.header "Graph View"
                                MenuItem.onClick (fun _ -> dispatch (SwitchView GraphView))
                            ]
                        ]
                    ]
                    MenuItem.create [
                        MenuItem.header "Scripts"
                    ]
                    MenuItem.create [
                        MenuItem.header "Settings"
                    ]

                    MenuItem.create [
                        MenuItem.header helpIconPath
                        MenuItem.tip """-- Graph View --
- Press ctrl + mouse wheel to zoom in/out
- Drag and drop devices to move them
- Double click on device to set connection point
- Right click on device ot toggle devices options menu
- Click on connection line to remove connection"""
                    ]
                ]
            ]

            Grid.create [
                Grid.rowDefinitions "*,*"
                Grid.children [
                    ContentControl.create [
                        Grid.row 0
                        ContentControl.content (
                            match model.View with
                            | ListView -> DeviceListView.view model.Devices dispatch
                            | GraphView -> DeviceGraphView.view model dispatch
                        )
                    ]
                    Border.create [
                        Grid.row 1
                        Border.borderThickness 1.0
                        Border.borderBrush "Gray"
                        Border.child (
                            DockPanel.create [
                                DockPanel.children [
                                    terminalTabView model.SelectedTerminalId model.Terminals dispatch
                                ]
                            ]
                        )
                    ]

                    if model.SetDeviceFormVisible then

                        Border.create [
                            Grid.row 0
                            Border.child (addDeviceForm (model.DeviceAcc.Value) dispatch)
                            Border.background "Gainsboro"
                        ]


                ]
            ]
        ]
    ]

