package main

import (
    "math/rand"
    "image"
    "image/draw"
    "image/color"
    "image/png"
    "os"
)

var (
    black = color.RGBA{0, 0, 0, 255}
    imw = 1920
    imh = 1080
    ws = float64(imw) / float64(w)
    hs = float64(imh) / float64(h)
)

func (q *QuadTree) Draw(i *image.RGBA) {
    col := color.RGBA{uint8(rand.Intn(255)), uint8(rand.Intn(255)), uint8(rand.Intn(255)), 255}

    for e := q.s.Front(); e != nil; e = e.Next() {
        o := e.Value.(*Object)
        /*
        fmt.Println(image.Rectangle{image.Point{o.pos.x * ws, o.pos.y * hs},
            image.Point{(o.pos.x + o.size.x) * ws, (o.pos.y + o.size.y) * hs}})
            */

        draw.Draw(i, image.Rectangle{
                image.Point{int(float64(o.pos.x) * ws), int(float64(o.pos.y) * hs)},
                image.Point{int(float64((o.pos.x + o.size.x)) * ws),
                int(float64((o.pos.y + o.size.y)) * hs)}},
            &image.Uniform{col}, image.ZP, draw.Src)
    }

    if q.NW != nil {
        q.NW.Draw(i)
        q.NE.Draw(i)
        q.SW.Draw(i)
        q.SE.Draw(i)
    }
}

func (q *QuadTree) DrawAreas (i *image.RGBA) {
    col2 := color.RGBA{uint8(rand.Intn(255)), uint8(rand.Intn(255)), uint8(rand.Intn(255)), 100}

    draw.Draw(i, image.Rectangle{
            image.Point{int(float64(q.r.lt.x) * ws), int(float64(q.r.lt.y) * hs)},
            image.Point{int(float64(q.r.rb.x) * ws), int(float64(q.r.rb.y) * hs)}},
        &image.Uniform{col2}, image.ZP, draw.Src)

    if q.NW != nil {
        q.NW.DrawAreas(i)
        q.NE.DrawAreas(i)
        q.SW.DrawAreas(i)
        q.SE.DrawAreas(i)
    }
}
func gollision_draw(q *QuadTree) {
    m := image.NewRGBA(image.Rect(0, 0, imw, imh))
    draw.Draw(m, m.Bounds(), &image.Uniform{black}, image.ZP, draw.Src)

    q.DrawAreas(m)
    q.Draw(m)

    f, _ := os.Create("out.png")
    png.Encode(f, m)

    /*qt.Print(1)*/
}
