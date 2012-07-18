package main

import (
    "fmt"
    "time"
    "math/rand"
    "strings"
    "image"
    "image/draw"
    "image/color"
    "image/png"
    "os"
)

var (
    w = 800
    h = 800
    // Need a better way to get maxdepth (also 0.5 -> 0.25)
    //maxdepth = int(math.Floor(math.Pow(float64(w), 0.5)))
    maxdepth = 5
    capacity = 30
    black = color.RGBA{0, 0, 0, 255}
    insert = 1000
    imw = 1920
    imh = 1080
    ws = float64(imw) / float64(w)
    hs = float64(imh) / float64(h)
    magic = 40
)

// Simple print of the tree
func (q *QuadTree) Print(d int) {
    prefix := strings.Repeat("-", d*2)

    fmt.Println(prefix, q.r, "#", q.s.Len())
    for e := q.s.Front(); e != nil; e = e.Next() {
        o := e.Value.(*Object)
        fmt.Println(prefix, " *", o)
    }

    if q.NW != nil {
        q.NW.Print(d+1)
        q.NE.Print(d+1)
        q.SW.Print(d+1)
        q.SE.Print(d+1)
    }
}

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

// Generate random objects and send them over the channel.
func GenerateObjects(c chan *Object) {
    var o *Object
    amt := 0
    for amt < insert {
        amt += 1
        x := rand.Intn(magic)
        y := rand.Intn(magic)
        if rand.Intn(20) > 18 {
            x = 1;
        }
        if rand.Intn(20) > 18 {
            y = 1;
        }
        o = &Object{Vertex{x, y}, Vertex{rand.Intn(w - magic),rand.Intn(h - magic)}}
        /*
        for {
            o = &Object{Vertex{rand.Intn(20), rand.Intn(20)},
                Vertex{rand.Intn(w),rand.Intn(h)}}
            if o.size.x + o.pos.x < 100 && o.size.y + o.pos.y < 100 {
                break
            }
        }
        */
        c <- o
    }

    close(c)
}



func main() {
    fmt.Println("Gollision!", w, h)

    R := Rectangle{Vertex{0, 0}, Vertex{w, h}}

    qt := new(QuadTree)

    c := make(chan *Object)

    t := time.Now()
    qt.Init(R, 0, maxdepth, capacity)

    go GenerateObjects(c)

    cc := 0
    for o := range c {
        cc += 1
        if !qt.Add(o) {
            fmt.Println("Failed to add:", o)
            qt.Print(0)
            fmt.Println("After", cc, "tries")
            //panic("Fail")
            break;
        }
    }

    t2 := time.Now()
    fmt.Println("Time taken to add", insert, "items: ", t2.Sub(t))

    m := image.NewRGBA(image.Rect(0, 0, imw, imh))
    draw.Draw(m, m.Bounds(), &image.Uniform{black}, image.ZP, draw.Src)

    qt.DrawAreas(m)
    qt.Draw(m)

    f, _ := os.Create("out.png")
    png.Encode(f, m)

    /*qt.Print(1)*/

    fmt.Println(maxdepth)
}
