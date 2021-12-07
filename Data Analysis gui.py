from tkinter import*
import pandas
import seaborn


root = Tk()
root.geometry('300x300')

l = Label(root, text='Uber Data Analysis')
l.pack()

global data

def Action():
    data = pandas.read_csv('C:/Users/hp/Documents/PROJECT/Uber_Data_Analysis 1/uber-raw-data-apr14.csv')
    data.head()        
    
b1 = Button(root, text='data',command=Action)
b1.pack()


root.mainloop()
