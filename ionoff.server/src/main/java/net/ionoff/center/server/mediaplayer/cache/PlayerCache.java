package net.ionoff.center.server.mediaplayer.cache;

abstract class PlayerCache {
	
	private long time;
	
	public long getTime() {
		return time;
	}
	
	public void setTime(long time) {
		this.time = time;
	}

	
	protected boolean isLiving() {
		
		long now = System.currentTimeMillis();
		if (now - time > getLivingTime()) {
			return false;
		}
		return true;
	}

	protected abstract long getLivingTime();
}
