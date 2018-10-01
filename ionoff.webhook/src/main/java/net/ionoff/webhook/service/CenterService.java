package net.ionoff.webhook.service;

import net.ionoff.webhook.model.Center;
import net.ionoff.webhook.repository.CenterRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Service
public class CenterService {

	@Autowired
    CenterRepository centerRepository;

	@Transactional
	public List<Center> getAllCenters() {
		return (List<Center>) centerRepository.findAll();
	}

	@Transactional
	public List<Center> findByKey(String key) {
		return centerRepository.findByKey(key);
	}

	@Transactional
	public Optional<Center> getById(Long id) {
		return centerRepository.findById(id);
	}

	@Transactional
	public void deleteById(Long id) {
		centerRepository.deleteById(id);
	}

	@Transactional
	public boolean addCenter(Center center) {
		return centerRepository.save(center) != null;
	}

	@Transactional
	public boolean updateCenter(Center center) {
		return centerRepository.save(center) != null;
	}
}
