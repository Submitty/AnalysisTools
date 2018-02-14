from PIL import Image
import check2_helper as help
im = Image.new('RGB', (1000, 360), 'white')
im2 = Image.open("1.jpg")
im3 = Image.open("2.jpg")
im4 = Image.open("3.jpg")
im5 = Image.open("4.jpg")
im6 = Image.open("5.jpg")
im7 = Image.open("6.jpg")

size1 = im2.size
size2 = im3.size
size3 = im4.size
size4 = im5.size
size5 = im6.size
size6 = im7.size

wallpaper = (256,256)

width1 = int(size1[0] * (wallpaper[1]/size1[1]))
width2 = int(size2[0] * (wallpaper[1]/size2[1]))
width3 = int(size3[0] * (wallpaper[1]/size3[1]))
width4 = int(size4[0] * (wallpaper[1]/size4[1]))
width5 = int(size5[0] * (wallpaper[1]/size5[1]))
width6 = int(size6[0] * (wallpaper[1]/size6[1]))

y = 20
z = 20 + 40

im.paste(im2.resize((width1,wallpaper[1])), (31,y))
a = 31 + width1 + 10
im.paste(im3.resize((width2,wallpaper[1])), (a,z))
b = a + width2 + 10
im.paste(im4.resize((width3,wallpaper[1])), (b,y))
c = b + width3 + 10
im.paste(im5.resize((width4,wallpaper[1])), (c,z))
d = c + width4 +10
im.paste(im6.resize((width5,wallpaper[1])), (d,y))
e = d + width5 + 10
im.paste(im7.resize((width6,wallpaper[1])), (e,z))
im.show()