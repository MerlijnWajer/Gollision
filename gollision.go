package main

import (
    "fmt"
    "math"
    "time"
    "container/list"
    "math/rand"
    "strings"
)

var (
    w = 100
    h = 100
    maxdepth = int(math.Floor(math.Pow(float64(w), 0.25)))
    capacity = 2
)

type Vertex struct {
    x, y int
}

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

type QuadTree struct {
    s *list.List
    r Rectangle
    depth int

    NW, NE, SW, SE *QuadTree
}

func (q *QuadTree) Init(r Rectangle, depth int) *QuadTree {
    q.s = new(list.List)
    q.s.Init()

    q.r = r
    q.depth = depth + 1

    /* NW, NE, SW, SE are nil initially */
    return q
}

func (q *QuadTree) SubDivide() bool {
    if q.depth > maxdepth {
        return false
    }
    w2 := (q.r.rb.x - q.r.lt.x) / 2
    h2 := (q.r.rb.y - q.r.lt.y) / 2

    NWArea := Rectangle{q.r.lt, Vertex{q.r.rb.x - w2, q.r.rb.y - h2}}

    NEArea := Rectangle{Vertex{q.r.lt.x + w2, q.r.lt.y},
        Vertex{q.r.rb.x, q.r.rb.y - h2}}

    SWArea := Rectangle{Vertex{q.r.lt.x, q.r.lt.y + h2},
        Vertex{q.r.rb.x - w2, q.r.rb.y}}

    SEArea := Rectangle{Vertex{q.r.lt.x + w2, q.r.lt.y + h2}, q.r.rb}

    q.NW = new(QuadTree).Init(NWArea, q.depth)
    q.NE = new(QuadTree).Init(NEArea, q.depth)
    q.SW = new(QuadTree).Init(SWArea, q.depth)
    q.SE = new(QuadTree).Init(SEArea, q.depth)

    return true
}

func (q *QuadTree) AddSimple(o * Object) bool {
    l := q.s
    if l.Len() >= capacity {
        return false
    }
    l.PushBack(o)
    return true
}

func (q *QuadTree) Clean() bool {
    l := new(list.List)
    l.Init()
    for e := q.s.Front(); e != nil; e = e.Next() {
        o := e.Value.(*Object)
        if !q.AddToChild(o) {
            l.PushBack(o)
        }
    }

    q.s = l

    return true
}

func (q *QuadTree) AddToChild(o *Object) bool {
    switch {
        case q.NW.CanContain(o): return q.NW.Add(o)
        case q.NE.CanContain(o): return q.NE.Add(o)
        case q.SE.CanContain(o): return q.SE.Add(o)
        case q.SW.CanContain(o): return q.SW.Add(o)
    }
    return false
}

func (q *QuadTree) Add(o *Object) bool {
    if !q.CanContain(o) {
        return false
    }
    if q.AddSimple(o) {
        return true
    }

    /* List if full, divide */
    if q.NW == nil {
        if !q.SubDivide() {
            return false
        }
        // Doesn't work due to other bug
        q.Clean()
    }

    success := q.AddToChild(o)


    if !success {
        return false
        //panic("Failed to add")
    }

    return success
}

func (q * QuadTree) CanContain(o *Object) bool {
    s := o.Size()
    p := o.Pos()
    return p.x >= q.r.lt.x && p.y >= q.r.lt.y &&
            s.x + p.x <= q.r.rb.x && s.y + p.y <= q.r.rb.y

}

func (q *QuadTree) Print(d int) {
    prefix := strings.Repeat("-", d*2)

    fmt.Println(prefix, q.r, "#=", q.s.Len())
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

func GenerateObjects(c chan *Object) {
    amt := 0
    for amt < 40 {
        amt += 1
        o := Object{Vertex{1, 1}, Vertex{rand.Intn(w), rand.Intn(h)}}
        //o := Object{Vertex{rand.Intn(50), rand.Intn(50)}, Vertex{rand.Intn(w), rand.Intn(h)}}
        //fmt.Println(o)
        c <- &o
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
        //qt.Print(0)
        //fmt.Println(strings.Repeat("-", 80))
    }
    qt.Print(0)

    t2 := time.Now()
    fmt.Println("Time taken: ", t2.Sub(t))

    fmt.Println(maxdepth)
}
