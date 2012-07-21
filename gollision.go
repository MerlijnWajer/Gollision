package main

import (
    "fmt"
    "time"
    "engine"
    "math/rand"
)

const (
    w = 1024
    h = 768
    magic = 10
    insert = 5000
)

// Generate random objects and send them over the channel.
func GenerateObjects(c chan *engine.Object) {
    var o *engine.Object
    amt := 0
    for amt < insert {
        amt += 1
        x := rand.Intn(magic)
        y := rand.Intn(magic)
        if rand.Intn(100) > 98 {
            x = 1;
        }
        if rand.Intn(100) > 98 {
            y = 1;
        }
        o = &engine.Object{
            Pos: engine.Vertex{rand.Intn(w - magic), rand.Intn(h - magic)},
            Size : engine.Vertex{x, y}}

        c <- o
    }

    close(c)
}

func main() {
    fmt.Println("Gollision")

    objchan := make(chan *engine.Object, 300)

    t := time.Now()

    qt := new(engine.QuadTree)
    qt.Init(engine.Rectangle{engine.Vertex{0, 0}, engine.Vertex{w, h}},
        0, &engine.QuadTreeInfo{MaxDepth: 5})

    go GenerateObjects(objchan)
    qt.AddObjects(objchan)

    t2 := time.Now()
    fmt.Println("Create time taken:", t2.Sub(t))

    t = time.Now()
    objchan = make(chan *engine.Object, 300)

    go func() {
        engine.Collisions(qt, objchan, 1)
        close(objchan)
    }()

    for _ = range objchan {
        // fmt.Println(o)
    }

    t2 = time.Now()
    fmt.Println("Collision time taken:", t2.Sub(t))

    //qt.Print(0)
}
