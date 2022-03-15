package factory;

import factory.*;

public class Factory {
    public static void main(String[] args) {
        CyclicQueue queue = new CyclicQueue(10000);

        var consumer1 = new Consumer(queue, 1);
        var producer1 = new Producer(queue, 1);

        Thread consumerThread1 = new Thread(consumer1);
        Thread producerThread1 = new Thread(producer1);

        producerThread1.start();
    }
}