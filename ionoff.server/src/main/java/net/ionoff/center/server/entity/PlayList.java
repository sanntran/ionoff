package net.ionoff.center.server.entity;

public class PlayList extends net.xapxinh.center.server.entity.PlayList implements IEntity {

	private static final long serialVersionUID = 1L;
	
	private User user;
	
	public User getUser() {
		return user;
	}
	
	public void setUser(User user) {
		this.user = user;
	}

	@Override
	public boolean isNew() {
		return getId() == 0;
	}	
	
}
