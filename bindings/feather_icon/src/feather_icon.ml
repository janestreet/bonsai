open! Core
open! Import

type t =
  | Activity
  | Corner_down_left
  | Link
  | Shopping_bag
  | Airplay
  | Corner_down_right
  | List
  | Shopping_cart
  | Alert_circle
  | Corner_left_down
  | Loader
  | Shuffle
  | Alert_octagon
  | Corner_left_up
  | Lock
  | Sidebar
  | Alert_triangle
  | Corner_right_down
  | Log_in
  | Skip_back
  | Align_center
  | Corner_right_up
  | Log_out
  | Skip_forward
  | Align_justify
  | Corner_up_left
  | Mail
  | Slack
  | Align_left
  | Corner_up_right
  | Map_pin
  | Slash
  | Align_right
  | Cpu
  | Map
  | Sliders
  | Anchor
  | Credit_card
  | Maximize_2
  | Smartphone
  | Aperture
  | Crop
  | Maximize
  | Smile
  | Archive
  | Crosshair
  | Meh
  | Speaker
  | Arrow_down_circle
  | Database
  | Menu
  | Square
  | Arrow_down_left
  | Delete
  | Message_circle
  | Star
  | Arrow_down_right
  | Disc
  | Message_square
  | Stop_circle
  | Arrow_down
  | Divide_circle
  | Mic_off
  | Sunrise
  | Arrow_left_circle
  | Divide_square
  | Mic
  | Sunset
  | Arrow_left
  | Divide
  | Minimize_2
  | Sun
  | Arrow_right_circle
  | Dollar_sign
  | Minimize
  | Tablet
  | Arrow_right
  | Download_cloud
  | Minus_circle
  | Tag
  | Arrow_up_circle
  | Download
  | Minus_square
  | Target
  | Arrow_up_left
  | Dribbble
  | Minus
  | Terminal
  | Arrow_up_right
  | Droplet
  | Monitor
  | Thermometer
  | Arrow_up
  | Edit_2
  | Moon
  | Thumbs_down
  | At_sign
  | Edit_3
  | More_horizontal
  | Thumbs_up
  | Award
  | Edit
  | More_vertical
  | Toggle_left
  | Bar_chart_2
  | External_link
  | Mouse_pointer
  | Toggle_right
  | Bar_chart
  | Eye_off
  | Move
  | Tool
  | Battery_charging
  | Eye
  | Music
  | Trash_2
  | Battery
  | Facebook
  | Navigation_2
  | Trash
  | Bell_off
  | Fast_forward
  | Navigation
  | Trello
  | Bell
  | Feather
  | Octagon
  | Trending_down
  | Bluetooth
  | Figma
  | Package
  | Trending_up
  | Bold
  | File_minus
  | Paperclip
  | Triangle
  | Bookmark
  | File_plus
  | Pause_circle
  | Truck
  | Book_open
  | File
  | Pause
  | Tv
  | Book
  | File_text
  | Pen_tool
  | Twitch
  | Box
  | Film
  | Percent
  | Twitter
  | Briefcase
  | Filter
  | Phone_call
  | Type
  | Calendar
  | Flag
  | Phone_forwarded
  | Umbrella
  | Camera_off
  | Folder_minus
  | Phone_incoming
  | Underline
  | Camera
  | Folder_plus
  | Phone_missed
  | Unlock
  | Cast
  | Folder
  | Phone_off
  | Upload_cloud
  | Check_circle
  | Framer
  | Phone_outgoing
  | Upload
  | Check_square
  | Frown
  | Phone
  | User_check
  | Check
  | Gift
  | Pie_chart
  | User_minus
  | Chevron_down
  | Git_branch
  | Play_circle
  | User_plus
  | Chevron_left
  | Git_commit
  | Play
  | Users
  | Chevron_right
  | Github
  | Plus_circle
  | User
  | Chevrons_down
  | Gitlab
  | Plus_square
  | User_x
  | Chevrons_left
  | Git_merge
  | Plus
  | Video_off
  | Chevrons_right
  | Git_pull_request
  | Pocket
  | Video
  | Chevrons_up
  | Globe
  | Power
  | Voicemail
  | Chevron_up
  | Grid
  | Printer
  | Volume_1
  | Chrome
  | Hard_drive
  | Radio
  | Volume_2
  | Circle
  | Hash
  | Refresh_ccw
  | Volume
  | Clipboard
  | Headphones
  | Refresh_cw
  | Volume_x
  | Clock
  | Heart
  | Repeat
  | Watch
  | Cloud_drizzle
  | Help_circle
  | Rewind
  | Wifi_off
  | Cloud_lightning
  | Hexagon
  | Rotate_ccw
  | Wifi
  | Cloud_off
  | Home
  | Rotate_cw
  | Wind
  | Cloud_rain
  | Image
  | Rss
  | X_circle
  | Cloud_snow
  | Inbox
  | Save
  | X_octagon
  | Cloud
  | Info
  | Scissors
  | X_square
  | Codepen
  | Instagram
  | Search
  | X
  | Codesandbox
  | Italic
  | Send
  | Youtube
  | Code
  | Key
  | Server
  | Zap_off
  | Coffee
  | Layers
  | Settings
  | Zap
  | Columns
  | Layout
  | Share_2
  | Zoom_in
  | Command
  | Life_buoy
  | Share
  | Zoom_out
  | Compass
  | Link_2
  | Shield_off
  | Copy
  | Linkedin
  | Shield
[@@deriving compare, enumerate, equal, sexp, sexp_grammar]

let path = function
  | Activity -> Paths.activity_dot_svg
  | Corner_down_left -> Paths.corner_down_left_dot_svg
  | Link -> Paths.link_dot_svg
  | Shopping_bag -> Paths.shopping_bag_dot_svg
  | Airplay -> Paths.airplay_dot_svg
  | Corner_down_right -> Paths.corner_down_right_dot_svg
  | List -> Paths.list_dot_svg
  | Shopping_cart -> Paths.shopping_cart_dot_svg
  | Alert_circle -> Paths.alert_circle_dot_svg
  | Corner_left_down -> Paths.corner_left_down_dot_svg
  | Loader -> Paths.loader_dot_svg
  | Shuffle -> Paths.shuffle_dot_svg
  | Alert_octagon -> Paths.alert_octagon_dot_svg
  | Corner_left_up -> Paths.corner_left_up_dot_svg
  | Lock -> Paths.lock_dot_svg
  | Sidebar -> Paths.sidebar_dot_svg
  | Alert_triangle -> Paths.alert_triangle_dot_svg
  | Corner_right_down -> Paths.corner_right_down_dot_svg
  | Log_in -> Paths.log_in_dot_svg
  | Skip_back -> Paths.skip_back_dot_svg
  | Align_center -> Paths.align_center_dot_svg
  | Corner_right_up -> Paths.corner_right_up_dot_svg
  | Log_out -> Paths.log_out_dot_svg
  | Skip_forward -> Paths.skip_forward_dot_svg
  | Align_justify -> Paths.align_justify_dot_svg
  | Corner_up_left -> Paths.corner_up_left_dot_svg
  | Mail -> Paths.mail_dot_svg
  | Slack -> Paths.slack_dot_svg
  | Align_left -> Paths.align_left_dot_svg
  | Corner_up_right -> Paths.corner_up_right_dot_svg
  | Map_pin -> Paths.map_pin_dot_svg
  | Slash -> Paths.slash_dot_svg
  | Align_right -> Paths.align_right_dot_svg
  | Cpu -> Paths.cpu_dot_svg
  | Map -> Paths.map_dot_svg
  | Sliders -> Paths.sliders_dot_svg
  | Anchor -> Paths.anchor_dot_svg
  | Credit_card -> Paths.credit_card_dot_svg
  | Maximize_2 -> Paths.maximize_2_dot_svg
  | Smartphone -> Paths.smartphone_dot_svg
  | Aperture -> Paths.aperture_dot_svg
  | Crop -> Paths.crop_dot_svg
  | Maximize -> Paths.maximize_dot_svg
  | Smile -> Paths.smile_dot_svg
  | Archive -> Paths.archive_dot_svg
  | Crosshair -> Paths.crosshair_dot_svg
  | Meh -> Paths.meh_dot_svg
  | Speaker -> Paths.speaker_dot_svg
  | Arrow_down_circle -> Paths.arrow_down_circle_dot_svg
  | Database -> Paths.database_dot_svg
  | Menu -> Paths.menu_dot_svg
  | Square -> Paths.square_dot_svg
  | Arrow_down_left -> Paths.arrow_down_left_dot_svg
  | Delete -> Paths.delete_dot_svg
  | Message_circle -> Paths.message_circle_dot_svg
  | Star -> Paths.star_dot_svg
  | Arrow_down_right -> Paths.arrow_down_right_dot_svg
  | Disc -> Paths.disc_dot_svg
  | Message_square -> Paths.message_square_dot_svg
  | Stop_circle -> Paths.stop_circle_dot_svg
  | Arrow_down -> Paths.arrow_down_dot_svg
  | Divide_circle -> Paths.divide_circle_dot_svg
  | Mic_off -> Paths.mic_off_dot_svg
  | Sunrise -> Paths.sunrise_dot_svg
  | Arrow_left_circle -> Paths.arrow_left_circle_dot_svg
  | Divide_square -> Paths.divide_square_dot_svg
  | Mic -> Paths.mic_dot_svg
  | Sunset -> Paths.sunset_dot_svg
  | Arrow_left -> Paths.arrow_left_dot_svg
  | Divide -> Paths.divide_dot_svg
  | Minimize_2 -> Paths.minimize_2_dot_svg
  | Sun -> Paths.sun_dot_svg
  | Arrow_right_circle -> Paths.arrow_right_circle_dot_svg
  | Dollar_sign -> Paths.dollar_sign_dot_svg
  | Minimize -> Paths.minimize_dot_svg
  | Tablet -> Paths.tablet_dot_svg
  | Arrow_right -> Paths.arrow_right_dot_svg
  | Download_cloud -> Paths.download_cloud_dot_svg
  | Minus_circle -> Paths.minus_circle_dot_svg
  | Tag -> Paths.tag_dot_svg
  | Arrow_up_circle -> Paths.arrow_up_circle_dot_svg
  | Download -> Paths.download_dot_svg
  | Minus_square -> Paths.minus_square_dot_svg
  | Target -> Paths.target_dot_svg
  | Arrow_up_left -> Paths.arrow_up_left_dot_svg
  | Dribbble -> Paths.dribbble_dot_svg
  | Minus -> Paths.minus_dot_svg
  | Terminal -> Paths.terminal_dot_svg
  | Arrow_up_right -> Paths.arrow_up_right_dot_svg
  | Droplet -> Paths.droplet_dot_svg
  | Monitor -> Paths.monitor_dot_svg
  | Thermometer -> Paths.thermometer_dot_svg
  | Arrow_up -> Paths.arrow_up_dot_svg
  | Edit_2 -> Paths.edit_2_dot_svg
  | Moon -> Paths.moon_dot_svg
  | Thumbs_down -> Paths.thumbs_down_dot_svg
  | At_sign -> Paths.at_sign_dot_svg
  | Edit_3 -> Paths.edit_3_dot_svg
  | More_horizontal -> Paths.more_horizontal_dot_svg
  | Thumbs_up -> Paths.thumbs_up_dot_svg
  | Award -> Paths.award_dot_svg
  | Edit -> Paths.edit_dot_svg
  | More_vertical -> Paths.more_vertical_dot_svg
  | Toggle_left -> Paths.toggle_left_dot_svg
  | Bar_chart_2 -> Paths.bar_chart_2_dot_svg
  | External_link -> Paths.external_link_dot_svg
  | Mouse_pointer -> Paths.mouse_pointer_dot_svg
  | Toggle_right -> Paths.toggle_right_dot_svg
  | Bar_chart -> Paths.bar_chart_dot_svg
  | Eye_off -> Paths.eye_off_dot_svg
  | Move -> Paths.move_dot_svg
  | Tool -> Paths.tool_dot_svg
  | Battery_charging -> Paths.battery_charging_dot_svg
  | Eye -> Paths.eye_dot_svg
  | Music -> Paths.music_dot_svg
  | Trash_2 -> Paths.trash_2_dot_svg
  | Battery -> Paths.battery_dot_svg
  | Facebook -> Paths.facebook_dot_svg
  | Navigation_2 -> Paths.navigation_2_dot_svg
  | Trash -> Paths.trash_dot_svg
  | Bell_off -> Paths.bell_off_dot_svg
  | Fast_forward -> Paths.fast_forward_dot_svg
  | Navigation -> Paths.navigation_dot_svg
  | Trello -> Paths.trello_dot_svg
  | Bell -> Paths.bell_dot_svg
  | Feather -> Paths.feather_dot_svg
  | Octagon -> Paths.octagon_dot_svg
  | Trending_down -> Paths.trending_down_dot_svg
  | Bluetooth -> Paths.bluetooth_dot_svg
  | Figma -> Paths.figma_dot_svg
  | Package -> Paths.package_dot_svg
  | Trending_up -> Paths.trending_up_dot_svg
  | Bold -> Paths.bold_dot_svg
  | File_minus -> Paths.file_minus_dot_svg
  | Paperclip -> Paths.paperclip_dot_svg
  | Triangle -> Paths.triangle_dot_svg
  | Bookmark -> Paths.bookmark_dot_svg
  | File_plus -> Paths.file_plus_dot_svg
  | Pause_circle -> Paths.pause_circle_dot_svg
  | Truck -> Paths.truck_dot_svg
  | Book_open -> Paths.book_open_dot_svg
  | File -> Paths.file_dot_svg
  | Pause -> Paths.pause_dot_svg
  | Tv -> Paths.tv_dot_svg
  | Book -> Paths.book_dot_svg
  | File_text -> Paths.file_text_dot_svg
  | Pen_tool -> Paths.pen_tool_dot_svg
  | Twitch -> Paths.twitch_dot_svg
  | Box -> Paths.box_dot_svg
  | Film -> Paths.film_dot_svg
  | Percent -> Paths.percent_dot_svg
  | Twitter -> Paths.twitter_dot_svg
  | Briefcase -> Paths.briefcase_dot_svg
  | Filter -> Paths.filter_dot_svg
  | Phone_call -> Paths.phone_call_dot_svg
  | Type -> Paths.type_dot_svg
  | Calendar -> Paths.calendar_dot_svg
  | Flag -> Paths.flag_dot_svg
  | Phone_forwarded -> Paths.phone_forwarded_dot_svg
  | Umbrella -> Paths.umbrella_dot_svg
  | Camera_off -> Paths.camera_off_dot_svg
  | Folder_minus -> Paths.folder_minus_dot_svg
  | Phone_incoming -> Paths.phone_incoming_dot_svg
  | Underline -> Paths.underline_dot_svg
  | Camera -> Paths.camera_dot_svg
  | Folder_plus -> Paths.folder_plus_dot_svg
  | Phone_missed -> Paths.phone_missed_dot_svg
  | Unlock -> Paths.unlock_dot_svg
  | Cast -> Paths.cast_dot_svg
  | Folder -> Paths.folder_dot_svg
  | Phone_off -> Paths.phone_off_dot_svg
  | Upload_cloud -> Paths.upload_cloud_dot_svg
  | Check_circle -> Paths.check_circle_dot_svg
  | Framer -> Paths.framer_dot_svg
  | Phone_outgoing -> Paths.phone_outgoing_dot_svg
  | Upload -> Paths.upload_dot_svg
  | Check_square -> Paths.check_square_dot_svg
  | Frown -> Paths.frown_dot_svg
  | Phone -> Paths.phone_dot_svg
  | User_check -> Paths.user_check_dot_svg
  | Check -> Paths.check_dot_svg
  | Gift -> Paths.gift_dot_svg
  | Pie_chart -> Paths.pie_chart_dot_svg
  | User_minus -> Paths.user_minus_dot_svg
  | Chevron_down -> Paths.chevron_down_dot_svg
  | Git_branch -> Paths.git_branch_dot_svg
  | Play_circle -> Paths.play_circle_dot_svg
  | User_plus -> Paths.user_plus_dot_svg
  | Chevron_left -> Paths.chevron_left_dot_svg
  | Git_commit -> Paths.git_commit_dot_svg
  | Play -> Paths.play_dot_svg
  | Users -> Paths.users_dot_svg
  | Chevron_right -> Paths.chevron_right_dot_svg
  | Github -> Paths.github_dot_svg
  | Plus_circle -> Paths.plus_circle_dot_svg
  | User -> Paths.user_dot_svg
  | Chevrons_down -> Paths.chevrons_down_dot_svg
  | Gitlab -> Paths.gitlab_dot_svg
  | Plus_square -> Paths.plus_square_dot_svg
  | User_x -> Paths.user_x_dot_svg
  | Chevrons_left -> Paths.chevrons_left_dot_svg
  | Git_merge -> Paths.git_merge_dot_svg
  | Plus -> Paths.plus_dot_svg
  | Video_off -> Paths.video_off_dot_svg
  | Chevrons_right -> Paths.chevrons_right_dot_svg
  | Git_pull_request -> Paths.git_pull_request_dot_svg
  | Pocket -> Paths.pocket_dot_svg
  | Video -> Paths.video_dot_svg
  | Chevrons_up -> Paths.chevrons_up_dot_svg
  | Globe -> Paths.globe_dot_svg
  | Power -> Paths.power_dot_svg
  | Voicemail -> Paths.voicemail_dot_svg
  | Chevron_up -> Paths.chevron_up_dot_svg
  | Grid -> Paths.grid_dot_svg
  | Printer -> Paths.printer_dot_svg
  | Volume_1 -> Paths.volume_1_dot_svg
  | Chrome -> Paths.chrome_dot_svg
  | Hard_drive -> Paths.hard_drive_dot_svg
  | Radio -> Paths.radio_dot_svg
  | Volume_2 -> Paths.volume_2_dot_svg
  | Circle -> Paths.circle_dot_svg
  | Hash -> Paths.hash_dot_svg
  | Refresh_ccw -> Paths.refresh_ccw_dot_svg
  | Volume -> Paths.volume_dot_svg
  | Clipboard -> Paths.clipboard_dot_svg
  | Headphones -> Paths.headphones_dot_svg
  | Refresh_cw -> Paths.refresh_cw_dot_svg
  | Volume_x -> Paths.volume_x_dot_svg
  | Clock -> Paths.clock_dot_svg
  | Heart -> Paths.heart_dot_svg
  | Repeat -> Paths.repeat_dot_svg
  | Watch -> Paths.watch_dot_svg
  | Cloud_drizzle -> Paths.cloud_drizzle_dot_svg
  | Help_circle -> Paths.help_circle_dot_svg
  | Rewind -> Paths.rewind_dot_svg
  | Wifi_off -> Paths.wifi_off_dot_svg
  | Cloud_lightning -> Paths.cloud_lightning_dot_svg
  | Hexagon -> Paths.hexagon_dot_svg
  | Rotate_ccw -> Paths.rotate_ccw_dot_svg
  | Wifi -> Paths.wifi_dot_svg
  | Cloud_off -> Paths.cloud_off_dot_svg
  | Home -> Paths.home_dot_svg
  | Rotate_cw -> Paths.rotate_cw_dot_svg
  | Wind -> Paths.wind_dot_svg
  | Cloud_rain -> Paths.cloud_rain_dot_svg
  | Image -> Paths.image_dot_svg
  | Rss -> Paths.rss_dot_svg
  | X_circle -> Paths.x_circle_dot_svg
  | Cloud_snow -> Paths.cloud_snow_dot_svg
  | Inbox -> Paths.inbox_dot_svg
  | Save -> Paths.save_dot_svg
  | X_octagon -> Paths.x_octagon_dot_svg
  | Cloud -> Paths.cloud_dot_svg
  | Info -> Paths.info_dot_svg
  | Scissors -> Paths.scissors_dot_svg
  | X_square -> Paths.x_square_dot_svg
  | Codepen -> Paths.codepen_dot_svg
  | Instagram -> Paths.instagram_dot_svg
  | Search -> Paths.search_dot_svg
  | X -> Paths.x_dot_svg
  | Codesandbox -> Paths.codesandbox_dot_svg
  | Italic -> Paths.italic_dot_svg
  | Send -> Paths.send_dot_svg
  | Youtube -> Paths.youtube_dot_svg
  | Code -> Paths.code_dot_svg
  | Key -> Paths.key_dot_svg
  | Server -> Paths.server_dot_svg
  | Zap_off -> Paths.zap_off_dot_svg
  | Coffee -> Paths.coffee_dot_svg
  | Layers -> Paths.layers_dot_svg
  | Settings -> Paths.settings_dot_svg
  | Zap -> Paths.zap_dot_svg
  | Columns -> Paths.columns_dot_svg
  | Layout -> Paths.layout_dot_svg
  | Share_2 -> Paths.share_2_dot_svg
  | Zoom_in -> Paths.zoom_in_dot_svg
  | Command -> Paths.command_dot_svg
  | Life_buoy -> Paths.life_buoy_dot_svg
  | Share -> Paths.share_dot_svg
  | Zoom_out -> Paths.zoom_out_dot_svg
  | Compass -> Paths.compass_dot_svg
  | Link_2 -> Paths.link_2_dot_svg
  | Shield_off -> Paths.shield_off_dot_svg
  | Copy -> Paths.copy_dot_svg
  | Linkedin -> Paths.linkedin_dot_svg
  | Shield -> Paths.shield_dot_svg
;;

let to_string t =
  sexp_of_t t
  |> Sexp.to_string
  |> String.lowercase
  |> String.tr ~target:'_' ~replacement:'-'
;;

let or_default_size size =
  Css_gen.Length.to_string_css
    (match size with
     | Some size -> (size :> Css_gen.Length.t)
     | None -> (`Px 24 :> Css_gen.Length.t))
;;

let or_default_stroke_width stroke_width =
  Css_gen.Length.to_string_css
    (match stroke_width with
     | Some stroke_width -> (stroke_width :> Css_gen.Length.t)
     | None -> (`Px 2 :> Css_gen.Length.t))
;;

let or_default_fill fill =
  match fill with
  | None -> "none"
  | Some color -> Css_gen.Color.to_string_css (color :> Css_gen.Color.t)
;;

let or_default_stroke stroke =
  Css_gen.Color.to_string_css
    (match stroke with
     | None -> (`Name "currentColor" :> Css_gen.Color.t)
     | Some stroke -> (stroke :> Css_gen.Color.t))
;;

let svg_string ?size ?stroke ?fill ?stroke_width (t : t) =
  let size = or_default_size size in
  let stroke_width = or_default_stroke_width stroke_width in
  let fill = or_default_fill fill in
  let stroke = or_default_stroke stroke in
  [%string
    {| <svg xmlns="http://www.w3.org/2000/svg" width="%{size}" height="%{size}" viewBox="0 0 24 24" fill="%{fill}" stroke="%{stroke}" stroke-width="%{stroke_width}" stroke-linecap="round" stroke-linejoin="round">%{path t}</svg> |}]
;;

let svg ?size ?stroke ?fill ?stroke_width ?(extra_attrs = []) (t : t) =
  let size = or_default_size size in
  let stroke_width = or_default_stroke_width stroke_width in
  let fill = or_default_fill fill in
  let stroke = or_default_stroke stroke in
  let module A = Vdom.Attr in
  let specific_class = "feather-" ^ to_string t in
  match Bonsai_web.am_running_how with
  | `Browser | `Browser_benchmark | `Node | `Node_benchmark ->
    Vdom.Node.inner_html_svg
      ~tag:"svg"
      ~attrs:
        ([ A.string_property "width" size
         ; A.string_property "height" size
         ; A.string_property "viewBox" "0 0 24 24"
         ; A.string_property "fill" fill
         ; A.string_property "stroke" stroke
         ; A.string_property "stroke-width" stroke_width
         ; A.string_property "stroke-linecap" "round"
         ; A.string_property "stroke-linejoin" "round"
         ; A.classes [ "feather"; specific_class ]
         ]
         @ extra_attrs)
      ~this_html_is_sanitized_and_is_totally_safe_trust_me:(path t)
      ()
  | `Node_test ->
    Vdom.Node.create ~attrs:extra_attrs [%string "feather_icons.%{to_string t}"] []
;;
