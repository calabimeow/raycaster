program main
    use iso_c_binding
    implicit none

    interface
        subroutine init_window(width, height, title) bind (C, name = "InitWindow")
            import :: c_int, c_char
            integer(c_int), value :: width
            integer(c_int), value :: height 
            character(kind = c_char), intent(in) :: title(*)
        end subroutine init_window 

        subroutine close_window() bind (C, name = "CloseWindow")
        end subroutine close_window

        subroutine set_target_fps(fps) bind (C, name = "SetTargetFPS")
            import :: c_int
            integer(c_int), value :: fps
        end subroutine set_target_fps

        logical(c_bool) function window_should_close() bind(C, name = "WindowShouldClose")
            import :: c_bool
        end function window_should_close

        subroutine begin_drawing() bind(C, name = "BeginDrawing")
        end subroutine begin_drawing 

        subroutine end_drawing() bind(C, name = "EndDrawing")
        end subroutine end_drawing

        subroutine clear_background(color) bind (C, name = "ClearBackground")
            import :: c_int32_t
            integer(c_int32_t), value :: color
        end subroutine clear_background

        subroutine draw_rectangle(x, y, w, h, color) bind(C, name = "DrawRectangle")
            import :: c_int, c_int32_t
            integer(c_int), value :: x, y, w, h
            integer(c_int32_t), value :: color
        end subroutine draw_rectangle

        function is_key_down(key) result(pressed) bind(C, name = "IsKeyDown")
            import :: c_int, c_bool
            integer(c_int), value :: key
            logical(c_bool) :: pressed
        end function is_key_down
    end interface

    integer(c_int32_t), parameter :: WHITE = int(Z"FFFFFFFF", c_int32_t)
    integer(c_int32_t), parameter :: BLACK = int(Z"FF000000", c_int32_t)
    integer(c_int32_t), parameter :: BLUE = int(Z"FFFF0000", c_int32_t)
    integer(c_int32_t), parameter :: DARK_BLUE = int(Z"FFBB0000", c_int32_t)

    real, parameter :: PI = 3.1415926535
    real, parameter :: P2 = PI / 2
    real, parameter :: P3 = 3 * PI / 2
    real, parameter :: DR = 0.0174533

    real :: player_x = 300.0, player_y = 300.0
    real :: player_angle = 0.0
    real :: player_dir_x = 5.0, player_dir_y = 0.0

    integer, parameter :: key_w = 87, key_s = 83, key_a = 65, key_d = 68

    integer, parameter :: map_size_x = 8, map_size_y = 8, map_size = 64
    integer :: map(map_size_x * map_size_y) = &
    [&
        1, 1, 1, 1, 1, 1, 1, 1, &
        1, 0, 1, 0, 0, 0, 0, 1, &
        1, 0, 1, 0, 0, 1, 0, 1, &
        1, 0, 1, 0, 0, 0, 0, 1, &
        1, 0, 0, 0, 0, 0, 0, 1, &
        1, 0, 0, 0, 0, 1, 0, 1, &
        1, 0, 1, 0, 0, 0, 0, 1, &
        1, 1, 1, 1, 1, 1, 1, 1  &
    ]

    integer, parameter:: screen_w = 800
    integer, parameter :: screen_h = 600

    call init_window(screen_w, screen_h, "raycaster")
    call set_target_fps(60)

    do while (.not. window_should_close())
        call update()
        call begin_drawing()
            call clear_background(BLACK)
            call draw()
        call end_drawing()
    end do

    call close_window()

contains
    subroutine update()
        if (is_key_down(key_w)) then
            player_x = player_x + player_dir_x
            player_y = player_y + player_dir_y
        end if

        if (is_key_down(key_s)) then
            player_x = player_x - player_dir_x
            player_y = player_y - player_dir_y
        end if

        if (is_key_down(key_a)) then
            player_angle = player_angle - 0.05
            if (player_angle < 0.0) player_angle = player_angle + 2.0 * PI
            player_dir_x = cos(player_angle) * 5.0
            player_dir_y = sin(player_angle) * 5.0
        end if

        if (is_key_down(key_d)) then
            player_angle = player_angle + 0.05
            if (player_angle > 2 * PI) player_angle = player_angle - 2.0 * PI
            player_dir_x = cos(player_angle) * 5.0
            player_dir_y = sin(player_angle) * 5.0
        end if
    end subroutine update

    subroutine draw()
        integer :: ray, map_x, map_y, map_index, dof, map_vert, map_horiz
        real :: ray_angle, ray_x, ray_y, offset_x, offset_y
        real :: dist_vert, dist_horiz, dist_total
        real :: atan, ntan, cam_angle, line_h, line_offset
        integer :: wall_color, draw_x
        integer :: rays_num, ray_width
        
        rays_num = 60
        ray_width = screen_w / rays_num
        if (ray_width < 1) ray_width = 1

        ray_angle = player_angle - DR * 30
        if (ray_angle < 0.0) ray_angle = ray_angle + 2.0 * PI
        if (ray_angle > 2.0 * PI) ray_angle = ray_angle - 2.0 * PI

        do ray = 0, rays_num
            draw_x = ray * ray_width

            !horiz lines
            dof = 0
            dist_horiz = 1000000.0
            atan = -1.0 / tan(ray_angle)
            
            if (ray_angle > PI) then
                ray_y = real(floor(player_y / real(map_size)) * map_size) - 0.0001
                ray_x = (player_y - ray_y) * atan + player_x
                offset_y = -real(map_size)
                offset_x = -offset_y * atan
            else if (ray_angle < PI) then
                ray_y = real(floor(player_y / real(map_size)) * map_size) + real(map_size)
                ray_x = (player_y - ray_y) * atan + player_x
                offset_y = real(map_size)
                offset_x = -offset_y * atan
            else
                ray_x = player_x
                ray_y = player_y
                dof = 8
            end if

            map_horiz = 0
            do while (dof < 8)
                map_x = floor(ray_x / real(map_size))
                map_y = floor(ray_y / real(map_size))
                
                if (map_x >= 0 .and. map_x < map_size_x .and. &
                    map_y >= 0 .and. map_y < map_size_y) then
                    map_index = map_y * map_size_x + map_x + 1

                    if (map(map_index) > 0) then
                        map_horiz = map(map_index)
                        dist_horiz = sqrt((ray_x - player_x) ** 2 + (ray_y - player_y) ** 2)
                        dof = 8
                        exit
                    end if
                end if
                
                ray_x = ray_x + offset_x
                ray_y = ray_y + offset_y
                dof = dof + 1
            end do
            
            !vert lines
            dof = 0
            dist_vert = 1000000.0
            ntan = -tan(ray_angle)
            
            if (ray_angle > P2 .and. ray_angle < P3) then
                ray_x = real(floor(player_x / real(map_size)) * map_size) - 0.0001
                ray_y = (player_x - ray_x) * ntan + player_y
                offset_x = -real(map_size)
                offset_y = -offset_x * ntan
            else if (ray_angle < P2 .or. ray_angle > P3) then
                ray_x = real(floor(player_x / real(map_size)) * map_size) + real(map_size)
                ray_y = (player_x - ray_x) * ntan + player_y
                offset_x = real(map_size)
                offset_y = -offset_x * ntan
            else
                ray_x = player_x
                ray_y = player_y
                dof = 8
            end if

            map_vert = 0
            do while (dof < 8)
                map_x = floor(ray_x / real(map_size))
                map_y = floor(ray_y / real(map_size))
                
                if (map_x >= 0 .and. map_x < map_size_x .and. &
                    map_y >= 0 .and. map_y < map_size_y) then
                    map_index = map_y * map_size_x + map_x + 1
                    if (map(map_index) > 0) then
                        map_vert = map(map_index)
                        dist_vert = sqrt((ray_x - player_x) ** 2 + (ray_y - player_y) ** 2)
                        dof = 8
                        exit
                    end if
                end if
                
                ray_x = ray_x + offset_x
                ray_y = ray_y + offset_y
                dof = dof + 1
            end do

            if (dist_vert < dist_horiz) then
                dist_total = dist_vert
                wall_color = BLUE
            end if

          if (dist_vert > dist_horiz) then
                dist_total = dist_horiz
                wall_color = DARK_BLUE
            end if

            cam_angle = player_angle - ray_angle
            if (cam_angle < 0.0) cam_angle = cam_angle + 2.0 * PI
            if (cam_angle > 2.0 * PI) cam_angle = cam_angle - 2.0 * PI
            dist_total = dist_total * cos(cam_angle)

            line_h = (real(map_size) * real(screen_h) / 2.0) / max(dist_total, 0.1)
            if (line_h > real(screen_h)) line_h = real(screen_h)
            line_offset = real(screen_h) / 2.0 - line_h / 2.0

            call draw_rectangle(draw_x, nint(line_offset), ray_width, nint(line_h), wall_color)

            ray_angle = ray_angle + DR
            if (ray_angle < 0.0) ray_angle = ray_angle + 2.0 * PI
            if (ray_angle > 2.0 * PI) ray_angle = ray_angle - 2.0 * PI
        end do
    end subroutine draw
end program main
