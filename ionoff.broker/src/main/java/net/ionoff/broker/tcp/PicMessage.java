package net.ionoff.broker.tcp;

public class PicMessage {
    private String keyId;
    private String digitalIn;
    private String relayOut;

    PicMessage(String message) {
        String messageItems[] = message.split(":");
        String dataItems[] = messageItems[1].split(",");
        if (dataItems.length == 3) {
            digitalIn = dataItems[0];
            relayOut = dataItems[1];
            keyId = dataItems[2];
        }
    }

    public String getKeyId() {
        return keyId;
    }
    public void setKeyId(String keyId) {
        this.keyId = keyId;
    }
    public String getDigitalIn() {
        return digitalIn;
    }
    public void setDigitalIn(String digitalIn) {
        this.digitalIn = digitalIn;
    }
    public String getRelayOut() {
        return relayOut;
    }
    public void setRelayOut(String relayOut) {
        this.relayOut = relayOut;
    }
}
