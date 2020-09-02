// Copyright 2020 Parity Technologies (UK) Ltd.
// This file is part of Polkadot.

// Polkadot is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Polkadot is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Polkadot.  If not, see <http://www.gnu.org/licenses/>.

//! The router module is responsible for handling messaging.
//!
//! The core of the messaging is checking and processing messages sent out by the candidates,
//! routing the messages at their destinations and informing the parachains about the incoming
//! messages.

use sp_std::prelude::*;
use frame_support::{decl_error, decl_module, decl_storage, weights::Weight};
use sp_runtime::traits::Hash;
use primitives::v1::{Id as ParaId, DownwardMessage, InboundDownwardMessage};

pub trait Trait: frame_system::Trait + crate::configuration::Trait {}

decl_storage! {
	trait Store for Module<T: Trait> as Router {
		/// Paras that are to be cleaned up at the end of the session.
		/// The entries are sorted ascending by the para id.
		OutgoingParas: Vec<ParaId>;

		/// The downward messages addressed for a certain para.
		DownwardMessageQueues: map hasher(twox_64_concat) ParaId => Vec<InboundDownwardMessage<T::AccountId, T::BlockNumber>>;
		/// A mapping that stores the downward message queue MQC head for each para.
		///
		/// Each link in this chain has a form:
		/// `(prev_head, B, H(M))`, where
		/// - `prev_head`: is the previous head hash or zero if none.
		/// - `B`: is the relay-chain block number in which a message was appended.
		/// - `H(M)`: is the hash of the message being appended.
		DownwardMessageQueueHeads: map hasher(twox_64_concat) ParaId => Option<T::Hash>;
	}
}

decl_error! {
	pub enum Error for Module<T: Trait> { }
}

decl_module! {
	/// The router module.
	pub struct Module<T: Trait> for enum Call where origin: <T as frame_system::Trait>::Origin {
		type Error = Error<T>;
	}
}

impl<T: Trait> Module<T> {
	/// Block initialization logic, called by initializer.
	pub(crate) fn initializer_initialize(_now: T::BlockNumber) -> Weight {
		0
	}

	/// Block finalization logic, called by initializer.
	pub(crate) fn initializer_finalize() {}

	/// Called by the initializer to note that a new session has started.
	pub(crate) fn initializer_on_new_session(
		_notification: &crate::initializer::SessionChangeNotification<T::BlockNumber>,
	) {
		let outgoing = OutgoingParas::take();
		for outgoing_para in outgoing {
			<Self as Store>::DownwardMessageQueues::remove(&outgoing_para);
			<Self as Store>::DownwardMessageQueueHeads::remove(&outgoing_para);
		}
	}

	/// Schedule a para to be cleaned up at the start of the next session.
	pub fn schedule_para_cleanup(id: ParaId) {
		OutgoingParas::mutate(|v| {
			if let Err(i) = v.binary_search(&id) {
				v.insert(i, id);
			}
		});
	}

	/// Enqueue a downward message to a specific recipient para.
	pub fn queue_downward_message(para: ParaId, msg: DownwardMessage<T::AccountId>) {
		let inbound = InboundDownwardMessage {
			msg,
			sent_at: <frame_system::Module<T>>::block_number(),
		};

		// Obtain the new link in the MQC and update the head.
		<Self as Store>::DownwardMessageQueueHeads::mutate(para, |head| {
			let prev_head = head.unwrap_or(Default::default());
			let new_head = T::Hashing::hash_of(&(
				prev_head,
				inbound.sent_at,
				T::Hashing::hash_of(&inbound.msg),
			));
			*head = Some(new_head);
		});

		<Self as Store>::DownwardMessageQueues::mutate(para, |v| {
			v.push(inbound);
		});
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use primitives::v1::BlockNumber;
	use frame_support::traits::{OnFinalize, OnInitialize};

	use crate::mock::{System, Router, new_test_ext};

	fn run_to_block(to: BlockNumber, new_session: Option<Vec<BlockNumber>>) {
		while System::block_number() < to {
			let b = System::block_number();
			Router::initializer_finalize();
			System::on_finalize(b);

			System::on_initialize(b + 1);
			System::set_block_number(b + 1);

			if new_session.as_ref().map_or(false, |v| v.contains(&(b + 1))) {
				Router::initializer_on_new_session(&Default::default());
			}
			Router::initializer_initialize(b + 1);
		}
	}

	#[test]
	fn scheduled_cleanup_performed() {
		let a = ParaId::from(1312);
		let b = ParaId::from(228);
		let c = ParaId::from(123);

		new_test_ext(Default::default()).execute_with(|| {
			run_to_block(1, None);

			// enqueue downward messages to A, B and C.
			Router::queue_downward_message(a, DownwardMessage::Opaque(vec![1, 2, 3]));
			Router::queue_downward_message(b, DownwardMessage::Opaque(vec![4, 5, 6]));
			Router::queue_downward_message(c, DownwardMessage::Opaque(vec![7, 8, 9]));

			Router::schedule_para_cleanup(a);

			// run to block without session change.
			run_to_block(2, None);

			assert!(!<Router as Store>::DownwardMessageQueues::get(&a).is_empty());
			assert!(!<Router as Store>::DownwardMessageQueues::get(&b).is_empty());
			assert!(!<Router as Store>::DownwardMessageQueues::get(&c).is_empty());

			Router::schedule_para_cleanup(b);

			run_to_block(3, Some(vec![3]));

			assert!(<Router as Store>::DownwardMessageQueues::get(&a).is_empty());
			assert!(<Router as Store>::DownwardMessageQueues::get(&b).is_empty());
			assert!(!<Router as Store>::DownwardMessageQueues::get(&c).is_empty());

			// verify that the outgoing paras are emptied.
			assert!(OutgoingParas::get().is_empty())
		});
	}
}
