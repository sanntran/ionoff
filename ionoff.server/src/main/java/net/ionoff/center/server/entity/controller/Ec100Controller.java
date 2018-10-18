package net.ionoff.center.server.entity.controller;

import java.util.Arrays;

import net.ionoff.center.server.entity.Controller;

public class Ec100Controller extends Controller {

    static {
        Controller.MODELS.put(model(), Ec100Controller.class);
    }

    private static String model() {
        return "HBQ_EC100";
    }

    @Override
    public int getInput() {
        return 28;
    }

    @Override
    public int getOutput() {
        return 28;
    }

    @Override
    public String getModel() {
        return model();
    }

    @Override
    public int getOnlineBuffer() {
        return 15000;
    }

    @Override
    public String getProtocol() {
        return "http";
    }

    @Override
    public boolean isLazy() {
        return true;
    }

    @Override
    public String getCommandStatus() {
        //$SS/0630,0590,0000,0000/1,1,1,1,z,z,z,z/0,0,0,0,z,z,z,z
        String urls[] = new String[4];
        urls[0] = ("http://" + getIp() + ":" + getPort() + "/ss.cgi?" + 1);
        urls[1] = ("http://" + getIp() + ":" + getPort() + "/ss.cgi?" + 2);
        urls[2] = ("http://" + getIp() + ":" + getPort() + "/ss.cgi?" + 3);
        urls[3] = ("http://" + getIp() + ":" + getPort() + "/ss.cgi?" + 4);
        return Arrays.toString(urls);
    }

    @Override
    public String getCommandOpenRelay(int relayIndex) {
        return "http://" + getIp() + ":" + getPort() + "/" + "zctl.cgi?"
                + getRoomId(relayIndex) + "" + getRelayId(relayIndex) + "=0&0=OFF";
    }

    @Override
    public String getCommandCloseRelay(int relayIndex) {
        return   "http://" + getIp() + ":" + getPort() + "/" + "zctl.cgi?"
                + getRoomId(relayIndex) + "" + getRelayId(relayIndex) + "=0&0=ON";
    }

    @Override
    public String getCommandOpenRelay(int relayIndex, Integer autoRevert) {
    	return getCommandOpenRelay(relayIndex);
    }

    @Override
    public String getCommandCloseRelay(int relayIndex, Integer autoRevert) {
    	if (autoRevert != null && autoRevert.intValue() == ONE_SECOND) {
        	return Arrays.toString(new String[] { 
        			getCommandCloseRelay(relayIndex), getCommandOpenRelay(relayIndex) });
    	}
    	return getCommandCloseRelay(relayIndex);
    }

    private int getRelayId(int relayIndex) {
        if (relayIndex < 4) {
            return relayIndex + 1;
        }
        if (relayIndex < 12) {
            return relayIndex - 4 + 1;
        }
        if (relayIndex < 20) {
            return relayIndex - 12 + 1;
        }
        // relayIndex < 28
        return relayIndex - 20 + 1;
    }

    private int getRoomId(int relayIndex) {
        if (relayIndex < 4) {
            return 1;
        }
        if (relayIndex < 12) {
            return 2;
        }
        if (relayIndex < 20) {
            return 3;
        }
        return 4;
    }

    @Override
    public void overrideKey() {
        setKey("EC" + (100000 + getId()));
    }

	@Override
	public int getOneSecondDelay() {
		return 900;
	}
}
