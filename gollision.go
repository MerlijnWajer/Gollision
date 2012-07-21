package main

import (
    "fmt"
    "time"
    "quadtree"
    "math/rand"
)

const (
    w = 800
    h = 800
    magic = 10
    insert = 100
)

// Generate random objects and send them over the channel.
func GenerateObjects(c chan *quadtree.Object) {
    var o *quadtree.Object
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
        o = &quadtree.Object{
            Pos: quadtree.Vertex{rand.Intn(w - magic), rand.Intn(h - magic)},
            Size : quadtree.Vertex{x, y}}

        c <- o
    }

    close(c)
}

func main() {
    fmt.Println("Gollision")

    objchan := make(chan *quadtree.Object)

    t := time.Now()

    qt := new(quadtree.QuadTree)
    qt.Init(quadtree.Rectangle{quadtree.Vertex{0, 0}, quadtree.Vertex{w, h}},
        0, &quadtree.QuadTreeInfo{MaxDepth: 5})

    go GenerateObjects(objchan)
    qt.AddObjects(objchan)

    t2 := time.Now()
    fmt.Println("Time taken:", t2.Sub(t))

    //qt.Print(0)
}
