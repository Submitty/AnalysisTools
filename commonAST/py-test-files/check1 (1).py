from PIL import Image
resize = (512//2, 512//2)

im = Image.new('RGB', (512,512), 'white')
im2 = Image.open("ca.jpg")
im3 = Image.open("tr.jpg")
im4 = Image.open("im.jpg")
im5 = Image.open("hk.jpg")
im.paste(im2.resize(resize), (0,0))
im.paste(im3.resize(resize), resize)
im.paste(im4.resize(resize), (resize[0],0))
im.paste(im5.resize(resize), (0,resize[1]))
im.show()