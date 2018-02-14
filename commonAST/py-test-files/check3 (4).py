from tkinter import *
from Ball import *
import random

colorList = ['blue', 'red', 'green', 'yellow', 'magenta', 'orange']

class BallDraw(object):
    def __init__ (self, parent):
        self.ball1 = Ball(random.randint(10, 390), random.randint(10, 390),\
                         random.randint(-8, 8), random.randint(-8, 8),\
                         random.randint(5, 10), random.choice(colorList))
        self.ball2 = Ball(random.randint(10, 390), random.randint(10, 390),\
                         random.randint(-8, 8), random.randint(-8, 8),\
                         random.randint(5, 10), random.choice(colorList))
        self.ball3 = Ball(random.randint(10, 390), random.randint(10, 390),\
                         random.randint(-8, 8), random.randint(-8, 8),\
                         random.randint(5, 10), random.choice(colorList))
        self.ball4 = Ball(random.randint(10, 390), random.randint(10, 390),\
                         random.randint(-8, 8), random.randint(-8, 8),\
                         random.randint(5, 10), random.choice(colorList))
        self.ball5 = Ball(random.randint(10, 390), random.randint(10, 390),\
                         random.randint(-8, 8), random.randint(-8, 8),\
                         random.randint(5, 10), random.choice(colorList))
        self.ball6 = Ball(random.randint(10, 390), random.randint(10, 390),\
                         random.randint(-8, 8), random.randint(-8, 8),\
                         random.randint(5, 10), random.choice(colorList))
        self.ball7 = Ball(random.randint(10, 390), random.randint(10, 390),\
                         random.randint(-8, 8), random.randint(-8, 8),\
                         random.randint(5, 10), random.choice(colorList))
        self.ball8 = Ball(random.randint(10, 390), random.randint(10, 390),\
                         random.randint(-8, 8), random.randint(-8, 8),\
                         random.randint(5, 10), random.choice(colorList))
        
        self.ball9 = Ball(random.randint(10, 390), random.randint(10, 390),\
                         random.randint(-8, 8), random.randint(-8, 8),\
                         random.randint(5, 10), random.choice(colorList))
        
        self.ball10 = Ball(random.randint(10, 390), random.randint(10, 390),\
                         random.randint(-8, 8), random.randint(-8, 8),\
                         random.randint(5, 10), random.choice(colorList))
        
        self.balls = [self.ball1, self.ball2, self.ball3, self.ball4, self.ball5, self.ball6, self.ball7, self.ball8, self.ball9, self.ball10]
        

        self.wait_time = 100 

        self.isstopped = False 

        self.maxx = 400 # canvas width, in pixels
        self.maxy = 400 # canvas height, in pixels

        self.parent = parent
        self.frame = Frame(parent)
        self.frame.pack()
        self.top_frame = Frame(self.frame)
        self.top_frame.pack(side=TOP)
        self.canvas = Canvas(self.top_frame, background="white", \
                             width=self.maxx, height=self.maxy )
        self.canvas.pack()
        self.bottom_frame = Frame(self.frame)
        self.bottom_frame.pack(side=BOTTOM)
        self.restart = Button(self.bottom_frame, text="Restart", command=self.restart)
        self.restart.pack(side=LEFT)
        self.slow = Button(self.bottom_frame, text="Slower", command=self.slower)
        self.slow.pack(side=LEFT)
        self.fast = Button(self.bottom_frame, text="Faster", command=self.faster)
        self.fast.pack(side=LEFT)
        self.quit = Button(self.bottom_frame, text="Quit", command=self.quit)
        self.quit.pack(side=RIGHT)

    def faster(self):
        if self.wait_time > 2:
            self.wait_time //= 2

    def slower(self):
        self.wait_time *= 2
            
    def restart(self):
        self.isstopped = False
        for ball in self.balls:
            ball.ball_x = ball.initialxy[0]
            ball.ball_y = ball.initialxy[1]
            ball.ball_dx = ball.initialdxdy[0]
            ball.ball_dy = ball.initialdxdy[1]
        self.animate()
        
    def quit(self):
        self.isstopped = True
        self.parent.destroy()
     
    def animate(self):
        ##  Loop until the ball runs off the screen.
        #  Remove all the previously-drawn balls       
        while not self.isstopped:
            self.canvas.delete("all") 
            for ball in self.balls:
                ball.move()
                ball.check_and_reverse(self.maxx, self.maxy)
                bounding_box = ball.bounding_box()
                self.canvas.create_oval(bounding_box, fill=ball.ball_color)                
                          
            self.canvas.update()      # Actually refresh the drawing on the canvas.   
     
            # Pause execution.  This allows the eye to catch up
            self.canvas.after(self.wait_time)         

if __name__ == "__main__":
    ##  We will create a root object, which will contain all 
    ##  our user interface elements
    ##
    root = Tk()
    root.title("Tkinter: Lab 11")

    ## Create a class to handle all our animation
    bd = BallDraw(root)

    ## Run the animation by continuously drawing the ball and then moving it
    bd.animate()

    ## This is an infinite loop that allows the window to listen to
    ## "events", which are user inputs.  The only user event here is
    ## closing the window, which ends the program. 
    root.mainloop()


    
