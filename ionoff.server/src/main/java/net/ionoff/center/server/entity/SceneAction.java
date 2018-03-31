package net.ionoff.center.server.entity;

public class SceneAction extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	public static final String NONE = "None";
	
	private String action;
	private SceneDevice sceneDevice;
	
	public String getAction() {
		return action;
	}
	public void setAction(String action) {
		this.action = action;
	}
	
	public SceneDevice getSceneDevice() {
		return sceneDevice;
	}
	public void setSceneDevice(SceneDevice sceneDevice) {
		this.sceneDevice = sceneDevice;
	}
	
	public boolean hasAction() {
		return action != null && !action.equals(NONE);
	}
}
