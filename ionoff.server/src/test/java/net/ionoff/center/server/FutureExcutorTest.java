package net.ionoff.center.server;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

public class FutureExcutorTest {

    private static final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);



    public static void main(String[] args) {
        System.out.println("Starting one-minute countdown now...");

        List<Runnable> runnableList = new ArrayList<>();

        for (int i = 0; i < 50; i++) {
            final int n = i;
            runnableList.add(new Runnable() {
                @Override
                public void run() {
                    // do the thing
                    System.out.println(n + " Out of time!");
                }});
        }


        for (int i = 0; i < 50; i++) {
            final int n = 50 - i;
            ScheduledFuture<?> countdown = scheduler.schedule(runnableList.get(i), n, TimeUnit.SECONDS);
        }



        //scheduler.shutdown();
    }
}
