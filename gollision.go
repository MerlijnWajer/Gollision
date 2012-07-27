package main

import (
	"engine"
	"fmt"
	"math/rand"
	"time"
    "sync"
)

const (
	w      = 1024
	h      = 768
	magic  = 20
	insert = 1000
)

var (
    player *engine.Object
    playerTree *engine.QuadTree
    enemyTree *engine.QuadTree
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
    wg := new(sync.WaitGroup)

    wg.Add(1)
    go func() {
        // Add new player bullets
        // Move players (based on input)
        // Move Player bullets
        wg.Done()
    }()

    wg.Add(1)
    go func() {
        // Add NPC bullets (AI)
        // Move NPCs
        // Move NPC bullets
        wg.Done()
    }()

    wg.Wait()
}

func Collide() {
    objchan := make(chan *engine.Object, 300)
	go func() {
		engine.FindCollision(enemyTree, player, objchan)
		close(objchan)
	}()

	for o := range objchan {
		o.Collides = true
		// fmt.Println(o)
	}
}

func Draw() {
    engine.Draw(enemyTree)
}

func main() {
	fmt.Println("Gollision")

	player = &engine.Object{Size: engine.Vertex{200, 200}, Pos: engine.Vertex{300, 300}}

    // TODO: 300 is random
	objchan := make(chan *engine.Object, 300)

	t := time.Now()

	playerTree = new(engine.QuadTree)
	enemyTree = new(engine.QuadTree)
	playerTree.Init(engine.Rectangle{engine.Vertex{0, 0}, engine.Vertex{w, h}},
		0, &engine.QuadTreeInfo{MaxDepth: 5})
	enemyTree.Init(engine.Rectangle{engine.Vertex{0, 0}, engine.Vertex{w, h}},
		0, &engine.QuadTreeInfo{MaxDepth: 5})

    // Generate some objects.
	go GenerateObjects(objchan)
	objchan <- player
	enemyTree.AddObjects(objchan)

	t2 := time.Now()
	fmt.Println("Create time taken:", t2.Sub(t))

    //engine.Draw_Init()

    t = time.Now()
    i := 0
    for i < 10000 {
        Move()
        Collide()
        //Draw()
        i += 1
    }
    t2 = time.Now()
    fmt.Println("BOOM:", t2.Sub(t) / 9999.0)

    //engine.Draw_Stop()

    /*
	t = time.Now()

    go func() {
        engine.Collisions(qt, objchan, 1)
        close(objchan)
    }()

	//go func() {
	//	engine.FindCollision(qt, oo, objchan)
	//	close(objchan)
	//}()

	for o := range objchan {
		o.Collides = true
		// fmt.Println(o)
	}

	t2 = time.Now()
	fmt.Println("Collision time taken:", t2.Sub(t))
    */

	//engine.SDLCall(qt)

	//qt.Print(0)
}
