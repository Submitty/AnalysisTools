from PIL import Image

def make_square(x):
    w,h = x.size
    if w>h:
        x_square = x.crop((0,0,h,h))
    elif h>w:
        x_square = x.crop((0, 0, w, w))
    return x_square    