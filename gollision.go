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
    //"io"
    "os"
)

var (
    w = 1000
    h = 1000
    // Need a better way to get maxdepth (also 0.5 -> 0.25)
    //maxdepth = int(math.Floor(math.Pow(float64(w), 0.5)))
    maxdepth = 10
    capacity = 5
    black = color.RGBA{0, 0, 0, 255}
    insert = 35
    imw = 1920
    imh = 1080
    ws = float64(imw) / float64(w)
    hs = float64(imh) / float64(h)
)

// Vertex represents a point
type Vertex struct {
    x, y int
}

// Rectangle is as you'd expect. Represented with left top and right bottom.
type Rectangle struct {
    lt, rb Vertex
}


type Object struct {
    size, pos Vertex
}

func (o *Object) Size() *Vertex {
    return &o.size
}

func (o *Object) Pos() *Vertex {
    return &o.pos
}

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
    col2 := color.RGBA{uint8(rand.Intn(255)), uint8(rand.Intn(255)), uint8(rand.Intn(255)), 100}

    draw.Draw(i, image.Rectangle{
            image.Point{int(float64(q.r.lt.x) * ws), int(float64(q.r.lt.y) * hs)},
            image.Point{int(float64(q.r.rb.x) * ws), int(float64(q.r.rb.y) * hs)}},
        &image.Uniform{col2}, image.ZP, draw.Src)

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
        fmt.Println("Drawing")
    }

    if q.NW != nil {
        q.NW.Draw(i)
        q.NE.Draw(i)
        q.SW.Draw(i)
        q.SE.Draw(i)
    }
}

// Generate random objects and send them over the channel.
func GenerateObjects(c chan *Object) {
    var o *Object
    amt := 0
    for amt < insert {
        amt += 1
        o = &Object{Vertex{5, 5}, Vertex{rand.Intn(w - 5),rand.Intn(h - 5)}}
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
    qt.Init(R, 0)

    go GenerateObjects(c)

    for o := range c {
        if !qt.Add(o) {
            fmt.Println("Failed to add:", o)
            qt.Print(0)
            panic("Fail")
        }
    }

    m := image.NewRGBA(image.Rect(0, 0, imw, imh))
    draw.Draw(m, m.Bounds(), &image.Uniform{black}, image.ZP, draw.Src)

    qt.Draw(m)

    f, _ := os.Create("out.png")
    png.Encode(f, m)


    t2 := time.Now()
    fmt.Println("Time taken to add", insert, "items: ", t2.Sub(t))

    qt.Print(1)

    fmt.Println(maxdepth)
}
