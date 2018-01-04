
from tkinter import *

class MyApp(object):
    def __init__(self, parent):
        self.width = 600
        self.height = 600
        self.parent = parent
        self.main_frame = Frame(parent)
        self.main_frame.pack()
        self.canvas_frame = Frame(self.main_frame)
        self.canvas_frame.pack(side=TOP)
        self.canvas = Canvas(self.main_frame, \
                             width=self.width, height=self.height)
        self.canvas.pack()
        
        self.button_frame = Frame(self.main_frame)
        self.button_frame.pack(side=BOTTOM)
        self.drawbutton = Button(self.button_frame, text="Draw", \
                                 command = self.draw)
        self.drawbutton.pack(side=LEFT)
        self.clearbutton = Button(self.button_frame, text="Clear", \
                                  command = self.clear)
        self.clearbutton.pack(side=LEFT)
        self.quitbutton = Button(self.button_frame, text="Quit", \
                                 command = self.quit)
        self.quitbutton.pack(side=RIGHT)
        
    def clear(self):
        self.canvas.delete("all")

    def quit(self):
        self.parent.destroy()

    def draw(self):
        self.draw_lines(self.width/2, self.height/2, \
                        min(self.width, self.height)/4 )


    ## Modify this function to call itself recursively to draw smaller
    ## plus signs at the four end points of the current plus sign.
    ## You must have a stopping condition to make sure that the
    ## recursion ends!
    def draw_lines(self, centerx, centery, length):
        self.canvas.create_line(centerx, centery+length, \
                                centerx, centery-length, fill="black") 
        self.canvas.create_line(centerx+length, centery, \
                                centerx-length, centery, fill="black") 
        self.canvas.update()
       # self.canvas.after(10)
        if length > 1:
            self.draw_lines(centerx - length, centery, length/2)
            self.draw_lines(centerx, centery + length, length/2)
            self.draw_lines(centerx + length, centery, length/2)
            self.draw_lines(centerx, centery - length, length/2)
        


### The main code simply creates a canvas and three buttons. 
if __name__ == "__main__":
    root = Tk()
    root.title("Recursion Example: Lab 12 Checkpoint 2")
    myapp = MyApp(root)
    root.mainloop()
