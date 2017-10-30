import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;

public class Main {

    public static class WriteCallback implements CompletionHandler<Integer, ByteBuffer> {
        final AsynchronousSocketChannel scAttachment;

        public WriteCallback(AsynchronousSocketChannel scAttachment) {
            this.scAttachment = scAttachment;
        }

        public void completed(Integer result, ByteBuffer bbAttachment) {
            if (bbAttachment.hasRemaining()) {
                scAttachment.write(bbAttachment, bbAttachment, this);
            } else {
                bbAttachment.clear();
            }
        }

        public void failed(Throwable t, ByteBuffer bbAttachment) {
            t.printStackTrace();
        }
    }

    public static class ReadCallback implements CompletionHandler<Integer, AsynchronousSocketChannel> {
        final ByteBuffer buffer = ByteBuffer.allocateDirect(1024);

        public void completed(Integer result, final AsynchronousSocketChannel scAttachment) {
            scAttachment.write((ByteBuffer) buffer.flip(), buffer, new WriteCallback(scAttachment));
        }

        public void failed(Throwable t, AsynchronousSocketChannel scAttachment) {
            t.printStackTrace();
        }

    }

    public static class ListenerCallback implements CompletionHandler<AsynchronousSocketChannel, Void> {

        final AsynchronousServerSocketChannel listener;

        public ListenerCallback(AsynchronousServerSocketChannel listener) {
            this.listener = listener;
        }

        public void completed(AsynchronousSocketChannel connection, Void v) {
            listener.accept(null, this);
            ReadCallback r = new ReadCallback();
            connection.read(r.buffer, connection, r);
        }

        public void failed(Throwable t, Void v) {
            t.printStackTrace();
        }
    }

    public static void main(String[] args) {

        try (final AsynchronousServerSocketChannel listener =
                     AsynchronousServerSocketChannel.open()) {

            listener.bind(new InetSocketAddress("localhost", 5437));

            while (true) {
                listener.accept(null, new ListenerCallback(listener));
                System.in.read(); // so we don't exit before a connection is established
            }

        } catch (IOException e) {
            e.printStackTrace();
        }

    }

}

