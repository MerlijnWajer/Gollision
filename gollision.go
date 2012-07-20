package main

import (
    "fmt"
    "time"
)

var (
    w = 800
    h = 800
    // Need a better way to get maxdepth (also 0.5 -> 0.25)
    //maxdepth = int(math.Floor(math.Pow(float64(w), 0.5)))
    maxdepth = 5
    capacity = 30
    insert = 50
    magic = 40
)


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

    _sdl(qt)

    //gollision_draw(qt)

    t2 := time.Now()
    fmt.Println("Time taken to add", insert, "items: ", t2.Sub(t))
}
