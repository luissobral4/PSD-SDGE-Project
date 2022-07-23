package Device;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Scanner;


public class Device {

    public static Map<String,List<String>> parseEvents(String path) throws FileNotFoundException {
        File f = new File(path);
        Scanner s = new Scanner(f);
        String[] tokens;
        Map<String,List<String>> res = new HashMap<>();

        while(s.hasNextLine()) {
            tokens = s.nextLine().split(":");
            String[] events = tokens[1].split(",");
            res.put(tokens[0], Arrays.asList(events));
            System.out.println(tokens[0]);
        }
        s.close();
        return res;
    }
    public static void main(String[] args) throws NumberFormatException, UnknownHostException, IOException, InterruptedException {

        if (args.length != 4){
            System.out.println("Argumentos inválidos");
            return;
        }
        int port = Integer.parseInt(args[0]);
        String id = args[1];
        String passwd = args[2];
        String type = args[3];
        
        Socket s = new Socket("localhost", port);
        DataOutputStream out = new DataOutputStream(s.getOutputStream());
        DataInputStream in = new DataInputStream(new BufferedInputStream(s.getInputStream()));

        Map<String,Integer> counter = new HashMap<>();

        out.write(("login " + id + " " + passwd + " " + type + "\n").getBytes());
        out.flush();

        byte[] arr = new byte[4096];
        int size = in.read(arr, 0, 4096);

        if(size > 0) {
            Map<String,List<String>> events = parseEvents("files/devices");
            
            int i = 0;
            while(i<100) {
                Thread.sleep(100);
                String e = events.get(type).get(new Random().nextInt(events.get(type).size()));
                counter.putIfAbsent(e, 0);
                counter.put(e,counter.get(e)+1);
                out.write(("event " + e + "\n").getBytes());
                out.flush();
                i++;
            }
            for(Map.Entry<String,Integer> entry : counter.entrySet()){
                System.out.println(entry.getKey() + " - " + entry.getValue());
            }
        }
        s.close();
    }
}
