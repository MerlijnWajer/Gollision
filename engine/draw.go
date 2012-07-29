package engine

import (
	"fmt"
	"github.com/0xe2-0x9a-0x9b/Go-SDL/sdl"
)

var (
	rmask uint32 = 0x000000ff
	gmask uint32 = 0x0000ff00
	bmask uint32 = 0x00ff0000
	amask uint32 = 0xff000000

	sw = 1024
	sh = 768

	ssw = float64(sw) / float64(sw)
	ssh = float64(sh) / float64(sh)
)
var screen *sdl.Surface
var surface *sdl.Surface
var surfaceb *sdl.Surface

func draw_sdl(o *Object, s *sdl.Surface) {
    var ss *sdl.Surface
    if o.Collides {
        ss = surfaceb
    } else {
        ss = surface
    }

    s.Blit(
        &sdl.Rect{int16(float64(o.Pos.X) * ssw),
            int16(float64(o.Pos.Y) * ssh),
            uint16(float64((o.Pos.X + o.Size.X)) * ssw),
            uint16(float64((o.Pos.Y + o.Size.Y)) * ssh)},
        ss,
        &sdl.Rect{0, 0, uint16(o.Size.X), uint16(o.Size.Y)})
}

func Draw_Init() {
	if sdl.Init(sdl.INIT_EVERYTHING) != 0 {
		fmt.Println(sdl.GetError())
	}

	screen = sdl.SetVideoMode(sw, sh, 32, sdl.RESIZABLE)
	if screen == nil {
		return
	}
	var video_info = sdl.GetVideoInfo()

	fmt.Println("HW_available = ", video_info.HW_available)
	fmt.Println("WM_available = ", video_info.WM_available)
	fmt.Println("Video_mem = ", video_info.Video_mem, "kiB")

	surface = sdl.CreateRGBSurface(sdl.HWSURFACE, 500, 500, 32, rmask,
		gmask, bmask, amask)

	surface.FillRect(nil, 0xff00ff00)

	surfaceb = sdl.CreateRGBSurface(sdl.HWSURFACE, 500, 500, 32, rmask,
		gmask, bmask, amask)

	surfaceb.FillRect(nil, 0xffff0000)
}

func Draw_Stop() {
    surface.Free()
    surfaceb.Free()
    screen.Free()
    sdl.Quit()
}

func Draw(o *Object) {
	//ticker := time.NewTicker(time.Second) // 50 Hz
	//ticker := time.NewTicker(time.Second / 50) // 50 Hz

    draw_sdl(o, screen)
}

func DrawFlip() {
    screen.Flip()
	screen.FillRect(nil, 0xff000000)
}

	//running := true
	//for running {
	//	select {
	//	case <-ticker.C:
	//		//screen.Blit(&sdl.Rect{0, 0, 100, 100}, surface, &sdl.Rect{0, 0, 100, 100})
	//		draw_sdl(q, screen)
	//		screen.Flip()

	//	case _event := <-sdl.Events:
	//		switch _event.(type) {
	//		case sdl.QuitEvent:
	//			running = false
	//		}
	//	}
	//}
