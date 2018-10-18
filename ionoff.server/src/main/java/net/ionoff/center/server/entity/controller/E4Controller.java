package net.ionoff.center.server.entity.controller;

import net.ionoff.center.server.entity.Controller;

public class E4Controller extends Controller {

    static {
        Controller.MODELS.put(model(), E4Controller.class);
    }

    private static String model() {
        return "IONOFF_E4";
    }

    @Override
    public int getInput() {
        return 4;
    }

    @Override
    public int getOutput() {
        return 4;
    }

    @Override
    public String getModel() {
        return model();
    }

    @Override
    public int getOnlineBuffer() {
        return 49000;
    }

    @Override
    public String getProtocol() {
        return "mqtt";
    }

    @Override
    public String getCommandStatus() {
        return "{ioget}";
    }

    @Override
    public String getCommandOpenRelay(int relayIndex) {
        return "{ioseto" + (relayIndex + 1) + "1}";
    }

    @Override
    public String getCommandCloseRelay(int relayIndex) {
        return "{ioseto" + (relayIndex + 1) + "0}";
    }

    @Override
    public String getCommandOpenRelay(int relayIndex, Integer autoRevert) {
        return getCommandOpenRelay(relayIndex);
    }

    @Override
    public String getCommandCloseRelay(int relayIndex, Integer autoRevert) {
    	if (autoRevert != null && autoRevert.intValue() == ONE_SECOND) {
    		return "{ioseto" + (relayIndex + 1) + "2}";
    	}
    	return getCommandCloseRelay(relayIndex);
        
    }

	@Override
	public int getOneSecondDelay() {
		return 650;
	}
}
