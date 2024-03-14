package net.ionoff.center.server.entity;

import org.hibernate.Hibernate;
import org.hibernate.proxy.HibernateProxy;

public final class EntityUtil {

	private EntityUtil() {
		// prevent installation
	}

	public static <T extends IEntity> boolean isInstance(T obj, Class<?> clazz) {
		if (obj instanceof HibernateProxy) {
			return (Hibernate.getClass(obj).equals(clazz));
		}
		return clazz.isInstance(obj);
	}

	@SuppressWarnings("unchecked")
	public static <T extends IEntity> T castUnproxy(IEntity obj, Class<T> clazz) {
		if (obj instanceof HibernateProxy) {
			final Object implObj = ((HibernateProxy) obj).getHibernateLazyInitializer()
					.getImplementation();
			if (clazz.isInstance(implObj)) {
				return (T)(implObj);
			}
		}
		else if (clazz.isInstance(obj)) {
			return (T)(obj);
		}
		throw new ClassCastException();
	}
}
