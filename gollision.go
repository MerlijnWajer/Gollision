package main

import (
    "container/list"
    "engine"
    "flag"
    "fmt"
    "math/rand"
    "sync"
    "time"
)

const (
    w     = 1024
    h     = 768
    magic = 20
)

var (
    player         *engine.Object
    playerTree     *engine.QuadTree
    enemyTree      *engine.QuadTree
    speedx, speedy int
    insert         int

    DrawChannel chan *engine.Object
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
            x = 1
        }
        if rand.Intn(100) > 98 {
            y = 1
        }
        o = &engine.Object{
            Pos:  engine.Vertex{rand.Intn(w - magic), rand.Intn(h - magic)},
            Size: engine.Vertex{x, y}}

        c <- o
    }

    close(c)
}

func Move() {
    //wg := new(sync.WaitGroup)

    enemyTree.Walk(
        func(e *list.Element) {
            b := e.Value.(*engine.Object)
            // TODO: set collides to false, move this somewhere else though
            b.Collides = false

            b.Pos.X = b.Pos.X + speedx
            b.Pos.Y = b.Pos.Y + speedy
            if b.Pos.X+b.Size.X > w {
                b.Pos.X = 0
            }
            if b.Pos.Y+b.Size.Y > h {
                b.Pos.Y = 0
            }
            enemyTree.Move(e)
            //fmt.Println(b)
        })

    //wg.Add(1)
    //go func() {
    //    // Add new player bullets
    //    // Move players (based on input)
    //    // Move Player bullets
    //    wg.Done()
    //}()

    //wg.Add(1)
    //go func() {
    //    // Add NPC bullets (AI)
    //    // Move NPCs
    //    // Move NPC bullets
    //    wg.Done()
    //}()

    //wg.Wait()
}

func Collide() {
    objchan := make(chan *engine.Object, 9001)

    go func() {
        engine.FindCollision(player.CurrentNode, player, objchan)
        close(objchan)
    }()

    for o := range objchan {
        o.Collides = true
    }

    // Send tree to draw channel. Obviously we need to do this
    // while performing collision.
    enemyTree.Walk(
        func(e *list.Element) {
            o := e.Value.(*engine.Object)
            DrawChannel <- o
        })
    close(DrawChannel)
}

func Draw() {
    for o := range DrawChannel {
        engine.Draw(o)
    }
    engine.DrawFlip()
}

func main() {
    flag.IntVar(&speedx, "speedx", 1, "Speed X")
    flag.IntVar(&speedy, "speedy", 1, "Speed Y")
    flag.IntVar(&insert, "insert", 1000, "Amount of bullets")
    flag.Parse()
    fmt.Println("Gollision")

    DrawChannel = make(chan *engine.Object, 300)

    player = &engine.Object{Size: engine.Vertex{200, 200}, Pos: engine.Vertex{300, 300}}

    // TODO: 300 is random
    objchan := make(chan *engine.Object, 300)

    t := time.Now()

    playerTree = new(engine.QuadTree)
    enemyTree = new(engine.QuadTree)
    playerTree.Init(engine.Rectangle{engine.Vertex{0, 0}, engine.Vertex{w, h}},
        0, &engine.QuadTreeInfo{MaxDepth: 5}, nil)
    enemyTree.Init(engine.Rectangle{engine.Vertex{0, 0}, engine.Vertex{w, h}},
        0, &engine.QuadTreeInfo{MaxDepth: 5}, nil)

    // Generate some objects.
    objchan <- player
    go GenerateObjects(objchan)
    enemyTree.AddObjects(objchan)

    t2 := time.Now()
    fmt.Println("Create time taken:", t2.Sub(t))

    engine.Draw_Init()
    go Draw()
    defer engine.Draw_Stop()

    t = time.Now()
    i := 0
    for i < 1000 {
        Move()

        DrawChannel = make(chan *engine.Object, 300)
        wg := new(sync.WaitGroup)
        wg.Add(1)
        go func() {
            Collide()
            wg.Done()
        }()
        wg.Add(1)
        go func() {
            Draw()
            wg.Done()
        }()
        wg.Wait()
        i += 1
    }
    t2 = time.Now()
    fmt.Println("BOOM:", t2.Sub(t)/999.0)
    fmt.Println("FPS:", float64(i)/(float64(t2.Sub(t))/float64(time.Second)))
}
