package main

import (
    "fmt"
    "github.com/0xe2-0x9a-0x9b/Go-SDL/sdl"
    "time"
)

var (
    rmask uint32 = 0x000000ff
    gmask uint32 = 0x0000ff00
    bmask uint32 = 0x00ff0000
    amask uint32 = 0xff000000

    sw = 1024
    sh = 768

    ssw = float64(sw) / float64(w)
    ssh = float64(sh) / float64(h)
)
var surface *sdl.Surface
var surfaceb *sdl.Surface


func draw_sdl(q *QuadTree, s *sdl.Surface) {

    for e := q.s.Front(); e != nil; e = e.Next() {
        o := e.Value.(*Object)
        var ss *sdl.Surface
        if o.collides {
            ss = surfaceb
        } else {
            ss = surface
        }

        /*
        fmt.Println(sdl.Rect{int16(float64(o.pos.x) * ssw),
            int16(float64(o.pos.y) * ssh),
            uint16(float64((o.pos.x + o.size.x)) * ssw),
            uint16(float64((o.pos.y + o.size.y)) * ssh)})
            */

        /*fmt.Println(o.size.x, o.size.y)*/
        s.Blit(
            &sdl.Rect{int16(float64(o.pos.x) * ssw),
            int16(float64(o.pos.y) * ssh),
            uint16(float64((o.pos.x + o.size.x)) * ssw),
            uint16(float64((o.pos.y + o.size.y)) * ssh)},
            ss,
            &sdl.Rect{0, 0, uint16(o.size.x), uint16(o.size.y)})
    }

    if q.NW != nil {
        draw_sdl(q.NW, s)
        draw_sdl(q.NE, s)
        draw_sdl(q.SW, s)
        draw_sdl(q.SE, s)
    }
}

func _sdl(q *QuadTree) {
    if sdl.Init(sdl.INIT_EVERYTHING) != 0 {
        fmt.Println(sdl.GetError())
    }

    var screen = sdl.SetVideoMode(sw, sh, 32, sdl.RESIZABLE)
    if screen == nil {
        return
    }
    var video_info = sdl.GetVideoInfo()

    fmt.Println("HW_available = ", video_info.HW_available)
    fmt.Println("WM_available = ", video_info.WM_available)
    fmt.Println("Video_mem = ", video_info.Video_mem, "kb")

    surface = sdl.CreateRGBSurface(sdl.HWSURFACE, 100, 100, 32, rmask,
        gmask, bmask, amask)

    surface.FillRect(nil, 0xff00ff00)

    surfaceb = sdl.CreateRGBSurface(sdl.HWSURFACE, 100, 100, 32, rmask,
        gmask, bmask, amask)

    surfaceb.FillRect(nil, 0xffff0000)

    /*
    screen.FillRect(nil, 0xffffffff)
    */

    ticker := time.NewTicker(time.Second / 10) // 50 Hz
    //ticker := time.NewTicker(time.Second / 50) // 50 Hz

    running := true;
    for running {
        select {
            case <- ticker.C:
                //screen.Blit(&sdl.Rect{0, 0, 100, 100}, surface, &sdl.Rect{0, 0, 100, 100})
                draw_sdl(q, screen)
                screen.Flip()

            case _event := <-sdl.Events:
                switch _event.(type) {
                    case sdl.QuitEvent:
                        running = false
                }
        }
    }

    surface.Free()
    sdl.Quit()
}

