package main

import (
    "fmt"
    "time"
    "container/list"
)

var (
    w = 1920
    h = 1080
    // Need a better way to get maxdepth (also 0.5 -> 0.25)
    //maxdepth = int(math.Floor(math.Pow(float64(w), 0.5)))
    maxdepth = 8
    capacity = 30
    insert = 1000
    magic = 20
)

func Collisions(q *QuadTree) {
    l := new(list.List)
    for e := q.s.Front(); e != nil; e = e.Next() {
        o := e.Value.(*Object)

        l.Init()
        FindCollision(q, o, l)
        if l.Len() > 0 {
            l.PushBack(o)
            for e2 := l.Front(); e2 != nil; e2 = e2.Next() {
                o2 := e2.Value.(*Object)
                o2.collides = true
            }
        }
    }

    if (q.NW != nil) {
        Collisions(q.NW)
        Collisions(q.NE)
        Collisions(q.SW)
        Collisions(q.SE)
    }
}

func Collides(o, o2 *Object) bool {
    switch {
        case o.pos.y + o.size.y < o2.pos.y:
            return false
        case o.pos.y > o2.pos.y + o2.size.y:
            return false
        case o.pos.x + o.size.x < o2.pos.x:
            return false
        case o.pos.x > o2.pos.x + o2.size.x:
            return false
    }

    return true
}

func FindCollision(q* QuadTree, obj *Object, l *list.List) {
    for e := q.s.Front(); e != nil; e = e.Next() {
        o := e.Value.(*Object)

        // We always collide with ourself
        if o == obj {
            continue
        }
        if Collides(o, obj) {
            //fmt.Println("Marking", o, "and", obj, "as collision")
            l.PushBack(o)
        }
    }

    if (q.NW != nil) {
        FindCollision(q.NW, obj, l)
        FindCollision(q.NE, obj, l)
        FindCollision(q.SW, obj, l)
        FindCollision(q.SE, obj, l)
    }
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

    qt.Clean() // This speeds up collision. ;-)
    t = time.Now()
    Collisions(qt)
    t2 = time.Now()
    fmt.Println("Time taken for collision", t2.Sub(t))

    _sdl(qt)

    //gollision_draw(qt)
}
