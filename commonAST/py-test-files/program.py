class draw_lines:
    def __init__(self):
        self.root = tk.Tk()
        self.chart = tk.Canvas(self.root, width=1280, height=760, background="white")
        self.chart.grid(row=0, column=0)
        self.root.frame = tk.Frame(self.root)
        self.root.frame.button = tk.Button(self.root.frame, text="quit", command=lambda:self.stop())
        self.root.frame.button.grid()
        self.root.frame.grid()
        self.isstopped = False
        self.restart()
        self.root.mainloop()
    def stop(self):
        self.isstopped = True
        self.root.destroy()
    def restart(self):
        locx = 120
        dx = 4
        angle = math.pi/6
        dangle = math.pi/72
        i=0
        while not self.isstopped:
            i += 1
            if i%2000 == 0:
                self.chart.delete(tk.ALL)
            dist = 400/math.sin(angle)
            x2 = locx+(dist*math.cos(angle))
            self.chart.create_line(locx,760,x2,0, fill="blue")
            self.chart.update()
            self.chart.after(60)
            locx += dx
            if locx > 1280:
                locx = 20 + abs(1280-locx)
            angle += dangle
            incr = 0.96
            dangle *= incr
            if dangle < (math.pi/72)/16:
                dangle = math.pi/72
            if angle >= 2*math.pi:
                angle = angle - 2*math.pi

import tkinter as tk
import math

draw_lines()
