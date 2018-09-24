package net.ionoff.broker.tcp;

public class PicMessage {
    private String keyId;

    public PicMessage(String message) {
        String messageItems[] = message.split(":");
        String dataItems[] = messageItems[1].split(",");
        if (dataItems.length == 3) {
            keyId = dataItems[2];
        }
    }

    public String getKeyId() {
        return keyId;
    }
    public void setKeyId(String keyId) {
        this.keyId = keyId;
    }
}
