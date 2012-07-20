package main

import (
    "math/rand"
)

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
