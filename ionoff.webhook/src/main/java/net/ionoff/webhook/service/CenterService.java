package net.ionoff.webhook.service;

import net.ionoff.webhook.dto.CenterDto;
import net.ionoff.webhook.mapper.CenterMapper;
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
	CenterMapper modelMapper;

	@Autowired
    CenterRepository centerRepository;

	@Transactional
	public CenterDto save(String id, CenterDto dto) {
		Center model;
		Optional<Center> optional = centerRepository.findById(id);
		if (optional.isPresent()) {
			model = optional.get();
		}
		else {
			model = new Center();
		}
		modelMapper.updateModel(dto, model);
		centerRepository.save(model);
		return modelMapper.createDto(model);
	}

	@Transactional
	public List<Center> findAll() {
		return (List<Center>) centerRepository.findAll();
	}

	@Transactional
	public Optional<Center> getById(String id) {
		return centerRepository.findById(id);
	}

	@Transactional
	public void deleteById(String id) {
		centerRepository.deleteById(id);
	}

}
