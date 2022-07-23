from fileinput import close
import socket
import sys
from threading import Thread
import threading
import time
import random


with open("files/devices") as f:
    lines = f.readlines()

dic = dict()
devices = list()

for line in lines:
    device = line.split(":")
    events = device[1].split(",")

    dic[device[0]] = events

class Device(Thread):
    def __init__(self,host,port,id,passwd,type):
        Thread.__init__(self)
        self.host = host
        self.port = port
        self.id = id
        self.passwd = passwd
        self.type = type
        self.socket = socket.socket(socket.AF_INET,socket.SOCK_STREAM)

    def sig_int_handler(self,signal,frame):
        pass

    def connect(self):
        self.socket.connect((self.host, self.port))
    
    def send(self,msg):
        self.socket.sendall(msg.encode('utf-8'))

    def recv(self,bytes):
        return self.socket.recv(bytes).decode('utf-8')

    def close(self):
        self.socket.close()

    def run(self):
        self.connect()

        msg = "login " + self.id + " " + self.passwd + " " + self.type + "\n"
        try:
            self.send(msg)

            resp = self.recv(1024)
            if "success" in resp.lower():
                print("success!!!")
            
                while(True):
                    sleep = random.uniform(1,2)
                    time.sleep(sleep)
                    event = random.choice(dic[self.type])
                    msg = "event " + event + "\n"
                    self.send(msg)
            else:
                print("Login failed")
        finally:
            print("closed!!!")
            self.close()
    
    def join(self):
        threading.Thread.join(self)

def main():
    args = sys.argv[1:]

    if len(args)==1 and args[0]=="-h":
        print("python3 device.py PORT")
    elif len(args)==2:
        HOST = "localhost"
        port = int(args[0])

        max = int(args[1])+1
        for i in range(1, max):
            id = "user" + str(i)
            passwd = "passwd" + str(i)
            type = random.choice(list(dic.keys()))

            device = Device(HOST,port,id,passwd,type)
            device.start()

            time.sleep(0.1)
    else:
        print("Wrong arguments")

main()    
