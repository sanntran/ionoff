package net.xapxinh.dataservice.persistence.service;

import java.io.Serializable;

import net.xapxinh.dataservice.persistence.IPersistence;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface IGenericService<T extends Serializable> extends IPersistence<T> {

}
