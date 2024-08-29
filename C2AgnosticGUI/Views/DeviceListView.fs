module SysAdminManagementApp.DeviceListView

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia

open SysAdminManagementApp.Models

let private createDeviceRow (device: Device): Types.IView =
    Grid.create [
        Grid.columnDefinitions "75, *, *, *, *"

        Grid.children [
            Image.create [
                Grid.column 0
                Image.source (device.OsType |> Option.map (fun osType -> osType.Logo) |> Option.defaultValue desktopBitmap)
            ]

            TextBlock.create [
                Grid.column 1
                TextBlock.text device.Name
                TextBlock.margin (Thickness 5.0)
            ]

            TextBlock.create [
                Grid.column 2
                TextBlock.text device.IP
                TextBlock.margin (Thickness 5.0)
            ]

            TextBlock.create [
                Grid.column 3
                TextBlock.text device.AssetName
                TextBlock.margin (Thickness 5.0)
            ]

            TextBlock.create [
                Grid.column 4
                TextBlock.text device.OsName
                TextBlock.margin (Thickness 5.0)
            ]
        ]
    ]

let view devices dispatch =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.children [
            Grid.create [
                Grid.columnDefinitions "100, *, *, *, *"
                Grid.children (
                    ["Os Logo"; "Name"; "IP"; "Asset Name"; "OS Name"]
                    |> List.mapi
                        (fun index header ->
                            TextBlock.create [
                                Grid.column index
                                TextBlock.text header
                                TextBlock.margin (Thickness 5.0)
                            ]
                            :> Types.IView
                        )
                )
            ]
            :> Types.IView

            yield! devices |> List.map createDeviceRow

            Button.create [
                Button.content plusIconPath
                Button.tip "Add Device"
                Button.onClick (fun _ -> dispatch (OnSetDeviceMsg (SetDeviceMsg.ToggleSetDeviceForm None)))
            ]
            :> Types.IView
        ]
    ]
    :> Types.IView