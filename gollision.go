package main

import (
    "fmt"
    "math"
    "time"
    "math/rand"
    "strings"
)

var (
    w = 100
    h = 100
    // Need a better way to get maxdepth (also 0.5 -> 0.25)
    maxdepth = int(math.Floor(math.Pow(float64(w), 0.5)))
    capacity = 10
    insert = 1000
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

// Generate random objects and send them over the channel.
func GenerateObjects(c chan *Object) {
    var o *Object
    amt := 0
    for amt < insert {
        amt += 1
        o = &Object{Vertex{1, 1}, Vertex{rand.Intn(w),rand.Intn(h)}}
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

    t2 := time.Now()
    fmt.Println("Time taken to add", insert, "items: ", t2.Sub(t))

    fmt.Println(maxdepth)
}
